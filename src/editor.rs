use std::io::{self, Write};

use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    execute, queue,
    style::{
        Attribute, Color, Print, ResetColor, SetAttribute, SetBackgroundColor, SetForegroundColor,
    },
    terminal,
};

use crate::ast::{Block, Expr, NamedExpr, NamedPattern, Path, Pattern};
use crate::lexer::{self, Lexer, Token, TokenKind};
use crate::parser::{self, Parser};
use crate::source::{Source, Span, Spanned};

// ---------------------------------------------------------------------------
// Token foreground color
// ---------------------------------------------------------------------------

fn token_color(kind: TokenKind) -> Color {
    match kind {
        // Literals
        TokenKind::True | TokenKind::False | TokenKind::Integer => Color::DarkRed,

        // Identifiers and paths
        TokenKind::Identifier | TokenKind::Underscore | TokenKind::DoubleColon => {
            Color::DarkMagenta
        }

        // Keywords and operators
        TokenKind::Else
        | TokenKind::Arrow
        | TokenKind::FatArrow
        | TokenKind::Exclamation
        | TokenKind::Interogation
        | TokenKind::And
        | TokenKind::Or
        | TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Times
        | TokenKind::Divide
        | TokenKind::Modulo
        | TokenKind::BitAnd
        | TokenKind::BitOr
        | TokenKind::BitXor
        | TokenKind::LShift
        | TokenKind::RShift
        | TokenKind::EQ
        | TokenKind::NE
        | TokenKind::GT
        | TokenKind::LT
        | TokenKind::GTE
        | TokenKind::LTE => Color::AnsiValue(130),

        TokenKind::Eof => Color::Reset,

        _ => Color::Black,
    }
}

// ---------------------------------------------------------------------------
// Lightweight AST cursor
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct AstNode {
    pub label: &'static str,
    pub span: Span,
}

// ---------------------------------------------------------------------------
// Compiler output
// ---------------------------------------------------------------------------

#[derive(Default)]
struct CompilerOutput {
    tokens: Vec<Spanned<Token>>,
    errors: Vec<Diagnostic>,
    ast: Option<Vec<Spanned<Expr>>>,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub span: Span,
    pub message: String,
}

// ---------------------------------------------------------------------------
// Editor state
// ---------------------------------------------------------------------------

pub struct Editor {
    source: Source,
    cursor: usize,
    output: CompilerOutput,
    ast_path: Vec<AstNode>,
    ast_cursor: Option<usize>,
    term_size: (u16, u16),
    scroll_row: usize,
}

impl Editor {
    pub fn new(text: &str) -> Self {
        Self::from_source(Source::new("<editor>".into(), text))
    }

    pub fn from_source(source: Source) -> Self {
        let term_size = terminal::size().unwrap_or((80, 24));
        let mut editor = Self {
            source,
            cursor: 0,
            output: CompilerOutput::default(),
            ast_cursor: None,
            ast_path: Vec::new(),
            term_size,
            scroll_row: 0,
        };
        editor.recompile();
        editor
    }

    // -----------------------------------------------------------------------
    // Main loop
    // -----------------------------------------------------------------------

    pub fn run(&mut self) -> io::Result<()> {
        let mut stdout = io::stdout();
        terminal::enable_raw_mode()?;
        execute!(stdout, terminal::EnterAlternateScreen, cursor::Hide)?;

        self.render(&mut stdout)?;

        loop {
            match event::read()? {
                Event::Key(key) => {
                    if self.handle_key(key) {
                        break;
                    }
                }
                Event::Resize(w, h) => {
                    self.term_size = (w, h);
                }
                _ => {}
            }
            self.render(&mut stdout)?;
        }

        execute!(stdout, terminal::LeaveAlternateScreen, cursor::Show)?;
        terminal::disable_raw_mode()?;
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Key handling
    // -----------------------------------------------------------------------

    fn handle_key(&mut self, key: KeyEvent) -> bool {
        let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
        let alt = key.modifiers.contains(KeyModifiers::ALT);

        if ctrl && key.code == KeyCode::Char('c') {
            return true;
        }

        if alt {
            match key.code {
                KeyCode::Up => {
                    self.ast_navigate_up();
                    return false;
                }
                KeyCode::Down => {
                    self.ast_navigate_down();
                    return false;
                }
                _ => {}
            }
        }

        match key.code {
            KeyCode::Left => {
                if alt {
                    if let Some(idx) = self.ast_cursor {
                        self.cursor = self.ast_path[idx].span.start;
                    } else {
                        self.move_node_left()
                    }
                } else if ctrl {
                    self.move_token_left()
                } else {
                    self.move_cursor_by(-1)
                }
            }
            KeyCode::Right => {
                if alt {
                    if let Some(idx) = self.ast_cursor {
                        self.cursor = self.ast_path[idx].span.end;
                    } else {
                        self.move_node_right()
                    }
                } else if ctrl {
                    self.move_token_right()
                } else {
                    self.move_cursor_by(1)
                }
            }
            KeyCode::Up => self.move_cursor_line(-1),
            KeyCode::Down => self.move_cursor_line(1),
            KeyCode::Home => self.move_cursor_home(),
            KeyCode::End => self.move_cursor_end(),

            KeyCode::Char(c) => {
                self.source.insert(self.cursor, c);
                self.cursor += 1;
                self.recompile();
            }
            KeyCode::Enter => {
                self.source.insert(self.cursor, '\n');
                self.cursor += 1;
                self.recompile();
            }
            KeyCode::Backspace => {
                if let Some(idx) = self.ast_cursor {
                    let span = self.ast_path[idx].span;
                    self.source.remove_span(span);
                    self.cursor = span.start;
                } else if self.cursor > 0 {
                    self.cursor -= 1;
                    self.source.remove(self.cursor);
                }
                self.recompile();
            }
            KeyCode::Delete => {
                if let Some(idx) = self.ast_cursor {
                    let span = self.ast_path[idx].span;
                    self.source.remove_span(span);
                    self.cursor = span.start;
                } else if self.cursor < self.source.len() {
                    self.source.remove(self.cursor);
                }
                self.recompile();
            }
            _ => {}
        }

        self.ast_cursor = None;
        if let Some(ast) = &self.output.ast {
            self.ast_path = ast_path_at(ast, self.cursor);
        }
        self.update_scroll();
        false
    }

    // -----------------------------------------------------------------------
    // Cursor movement
    // -----------------------------------------------------------------------

    fn move_node_left(&mut self) {
        if let Some(node) = self.ast_path.last() {
            self.cursor = node.span.start;
        } else {
            self.move_token_left()
        }
    }

    fn move_node_right(&mut self) {
        if let Some(node) = self.ast_path.last() {
            self.cursor = node.span.end.saturating_sub(1).max(node.span.start);
        } else {
            self.move_token_right()
        }
    }

    fn move_token_left(&mut self) {
        let pos = self.cursor;
        let tokens = &self.output.tokens;
        let idx = tokens.partition_point(|tok| tok.span.start < pos);
        if let Some(tok) = tokens.get(idx.saturating_sub(1)) {
            if self.cursor <= tok.span.start {
                self.cursor = 0;
            } else {
                self.cursor = tok.span.start;
            }
        }
    }

    fn move_token_right(&mut self) {
        let pos = self.cursor;
        let tokens = &self.output.tokens;
        let idx = tokens.partition_point(|tok| tok.span.start <= pos);
        if let Some(tok) = tokens.get(idx) {
            self.cursor = tok.span.start;
        }
    }

    fn move_cursor_by(&mut self, delta: isize) {
        self.cursor = (self.cursor as isize + delta).clamp(0, self.source.len() as isize) as usize;
    }

    fn move_cursor_line(&mut self, delta: isize) {
        let pos = self.source.index_to_position(self.cursor);
        let new_row =
            (pos.row as isize + delta).clamp(0, self.source.line_count() as isize - 1) as usize;
        self.cursor = self.source.position_to_index(new_row, pos.col);
    }

    fn move_cursor_home(&mut self) {
        self.cursor = self.source.index_to_position(self.cursor).line_start;
    }

    fn move_cursor_end(&mut self) {
        self.cursor = self.source.index_to_position(self.cursor).line_end;
    }

    fn update_scroll(&mut self) {
        let row = self.source.index_to_position(self.cursor).row;
        let visible = self.content_rows() as usize;
        if row < self.scroll_row {
            self.scroll_row = row;
        } else if row >= self.scroll_row + visible {
            self.scroll_row = row + 1 - visible;
        }
    }

    fn content_rows(&self) -> u16 {
        let reserved = 2u16 + self.diagnostic_line_count() as u16;
        self.term_size.1.saturating_sub(reserved)
    }

    // -----------------------------------------------------------------------
    // AST navigation
    // -----------------------------------------------------------------------

    fn ast_navigate_up(&mut self) {
        let len = self.ast_path.len();
        self.ast_cursor = match (self.ast_cursor, len) {
            (_, 0) => None,
            (Some(i), _) => Some(i.saturating_sub(1)),
            (None, _) => Some(len - 1),
        };
    }

    fn ast_navigate_down(&mut self) {
        let len = self.ast_path.len();
        self.ast_cursor = match (self.ast_cursor, len) {
            (_, 0) => None,
            (Some(i), _) => Some((i + 1).min(len - 1)),
            (None, _) => Some(len - 1),
        };
    }

    // -----------------------------------------------------------------------
    // Recompile
    // -----------------------------------------------------------------------

    fn recompile(&mut self) {
        let mut errors: Vec<Diagnostic> = Vec::new();
        let mut ast = self.output.ast.take();

        let mut lexer = Lexer::new(&self.source);
        let tokens = match lexer.tokenize() {
            Ok(tokens) => tokens,
            Err(error) => {
                let (span, message) = lex_error_info(&error);
                errors.push(Diagnostic { span, message });
                self.output.errors = errors;
                return;
            }
        };

        let mut parser = Parser::new(&self.source, tokens.clone());
        match parser.parse() {
            Ok(exprs) => ast = Some(exprs),
            Err(error) => {
                let (span, message) = parser_error_info(&error);
                errors.push(Diagnostic { span, message });
            }
        }

        self.output = CompilerOutput {
            tokens,
            errors,
            ast,
        };
    }

    // -----------------------------------------------------------------------
    // Rendering
    // -----------------------------------------------------------------------

    fn render<W: Write>(&self, stdout: &mut W) -> io::Result<()> {
        let cols = self.term_size.0 as usize;
        let content_rows = self.content_rows() as usize;
        let diag_rows = self.diagnostic_line_count();

        let highlight_span: Option<Span> = self.ast_cursor.map(|idx| self.ast_path[idx].span);

        let header = format!(
            " Terse2 IDE  |  {}  |  Ctrl-C: quit  |  Alt-↑/↓: AST nav",
            self.source.name
        );
        queue!(
            stdout,
            cursor::MoveTo(0, 0),
            SetForegroundColor(Color::White),
            SetBackgroundColor(Color::DarkBlue),
            Print(pad(&header, cols)),
            ResetColor,
        )?;

        const GUTTER: usize = 4;
        let text_cols = cols.saturating_sub(GUTTER);

        let source_lines: Vec<(usize, usize)> = self
            .source
            .lines()
            .enumerate()
            .filter(|(i, _)| *i >= self.scroll_row && *i < self.scroll_row + content_rows)
            .map(|(_, b)| b)
            .collect();

        for (screen_row, (line_start, line_end)) in source_lines.iter().enumerate() {
            let y = screen_row as u16 + 1;
            queue!(
                stdout,
                cursor::MoveTo(0, y),
                SetForegroundColor(Color::AnsiValue(130)),
                Print(format!("{:3} ", self.scroll_row + screen_row + 1)),
                ResetColor,
            )?;

            self.render_line(stdout, *line_start, *line_end, text_cols, highlight_span)?;
        }

        for blank in source_lines.len()..content_rows {
            queue!(
                stdout,
                cursor::MoveTo(0, blank as u16 + 1),
                Print(format!("{:width$}", "", width = cols)),
            )?;
        }

        let diag_top = 1 + content_rows as u16;
        self.render_diagnostics(stdout, diag_top, cols, diag_rows)?;

        let status_y = self.term_size.1.saturating_sub(1);
        queue!(
            stdout,
            cursor::MoveTo(0, status_y),
            SetBackgroundColor(Color::DarkBlue),
            SetForegroundColor(Color::White),
            Print(pad(&self.build_status(), cols)),
            ResetColor,
        )?;

        stdout.flush()
    }

    fn render_line<W: Write>(
        &self,
        stdout: &mut W,
        line_start: usize,
        line_end: usize,
        max_cols: usize,
        highlight_span: Option<Span>,
    ) -> io::Result<()> {
        let tokens = &self.output.tokens;
        let mut col = 0usize;
        let mut offset = line_start;
        let mut tok_idx = tokens.partition_point(|tok| tok.span.end <= line_start);

        while offset < line_end && col < max_cols {
            let next_tok_start = tokens
                .get(tok_idx)
                .map(|tok| tok.span.start)
                .unwrap_or(usize::MAX);

            if offset < next_tok_start {
                let gap_end = next_tok_start.min(line_end);
                col += self.render_segment(
                    stdout,
                    offset,
                    gap_end,
                    Color::DarkGrey,
                    highlight_span,
                    col,
                    max_cols,
                )?;
                offset = gap_end;
                continue;
            }

            let Some(tok) = tokens.get(tok_idx) else {
                break;
            };
            if tok.span.start >= line_end {
                break;
            }

            let tok_end = tok.span.end.min(line_end);
            col += self.render_segment(
                stdout,
                offset,
                tok_end,
                token_color(tok.kind()),
                highlight_span,
                col,
                max_cols,
            )?;
            offset = tok_end;
            tok_idx += 1;
        }

        if self.cursor == line_end && col < max_cols {
            queue!(
                stdout,
                SetBackgroundColor(Color::Black),
                Print(' '),
                ResetColor
            )?;
            col += 1;
        }

        if col < max_cols {
            queue!(
                stdout,
                Print(format!("{:width$}", "", width = max_cols - col))
            )?;
        }

        Ok(())
    }

    fn render_segment<W: Write>(
        &self,
        stdout: &mut W,
        start: usize,
        end: usize,
        base_color: Color,
        highlight_span: Option<Span>,
        col_offset: usize,
        max_cols: usize,
    ) -> io::Result<usize> {
        let cursor_inside = self.cursor >= start && self.cursor < end;
        let hl_inside = highlight_span
            .map(|s| s.start < end && s.end > start)
            .unwrap_or(false);
        let err_inside = self
            .output
            .errors
            .iter()
            .any(|e| e.span.start < end && e.span.end > start);

        if !cursor_inside && !hl_inside && !err_inside {
            let len = (max_cols - col_offset).min(end - start);
            if len == 0 {
                return Ok(0);
            }
            let text = self.source.substring(start, start + len);
            queue!(
                stdout,
                SetForegroundColor(base_color),
                Print(&text),
                ResetColor
            )?;
            return Ok(len);
        }

        let mut count = 0;
        for offset in start..end {
            if col_offset + count >= max_cols {
                break;
            }
            let c = self.source.buffer[offset];
            let is_cursor = offset == self.cursor;
            let is_hl = highlight_span.map(|s| s.contains(offset)).unwrap_or(false);
            let is_err = self.output.errors.iter().any(|e| e.span.contains(offset));

            if is_cursor {
                queue!(
                    stdout,
                    SetBackgroundColor(base_color),
                    SetForegroundColor(Color::White),
                    Print(c),
                    ResetColor,
                )?;
            } else if is_hl {
                queue!(
                    stdout,
                    SetBackgroundColor(Color::Grey),
                    SetForegroundColor(base_color),
                    Print(c),
                    ResetColor,
                )?;
            } else if is_err {
                queue!(
                    stdout,
                    SetForegroundColor(Color::White),
                    SetBackgroundColor(Color::Red),
                    Print(c),
                    ResetColor,
                )?;
            } else {
                queue!(stdout, SetForegroundColor(base_color), Print(c), ResetColor)?;
            }
            count += 1;
        }
        Ok(count)
    }

    // -----------------------------------------------------------------------
    // Diagnostics
    // -----------------------------------------------------------------------

    fn diagnostic_line_count(&self) -> usize {
        if self.output.errors.is_empty() {
            0
        } else {
            (1 + self.output.errors.len()).min(6)
        }
    }

    fn render_diagnostics<W: Write>(
        &self,
        stdout: &mut W,
        top_y: u16,
        cols: usize,
        rows: usize,
    ) -> io::Result<()> {
        if rows == 0 {
            return Ok(());
        }

        let n = self.output.errors.len();
        let header = format!(" {} error{}", n, if n == 1 { "" } else { "s" });
        queue!(
            stdout,
            cursor::MoveTo(0, top_y),
            SetBackgroundColor(Color::DarkRed),
            SetForegroundColor(Color::White),
            SetAttribute(Attribute::Bold),
            Print(pad(&header, cols)),
            SetAttribute(Attribute::Reset),
            ResetColor,
        )?;

        for (i, diag) in self.output.errors.iter().enumerate().take(rows - 1) {
            let pos = self.source.index_to_position(diag.span.start);
            let line = format!("  {}:{} — {}", pos.row + 1, pos.col + 1, diag.message);
            queue!(
                stdout,
                cursor::MoveTo(0, top_y + 1 + i as u16),
                SetForegroundColor(Color::Red),
                Print(pad(&line, cols)),
                ResetColor,
            )?;
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Status bar
    // -----------------------------------------------------------------------

    fn build_status(&self) -> String {
        let pos = self.source.index_to_position(self.cursor);
        let loc = format!("{}:{}", pos.row + 1, pos.col + 1);

        let token_info = {
            let tokens = &self.output.tokens;
            let idx = tokens.partition_point(|tok| tok.span.end <= self.cursor);
            tokens
                .get(idx)
                .filter(|tok| tok.contains(self.cursor))
                .map(|tok| format!("{:?}", tok.kind()))
                .unwrap_or_else(|| "—".into())
        };

        let ast_info = if let Some(idx) = self.ast_cursor {
            let node = &self.ast_path[idx];
            format!(
                "AST [{}/{}] {}  {}..{}",
                idx + 1,
                self.ast_path.len(),
                node.label,
                node.span.start,
                node.span.end,
            )
        } else if let Some(deepest) = self.ast_path.last() {
            format!("AST: {}", deepest.label)
        } else {
            "AST: —".into()
        };

        let ok = if self.output.errors.is_empty() {
            "✓"
        } else {
            ""
        };
        format!(" {} | {} | {} | {} ", loc, token_info, ast_info, ok)
    }
}

// ---------------------------------------------------------------------------
// AST traversal
// ---------------------------------------------------------------------------

fn ast_path_at(exprs: &[Spanned<Expr>], cursor: usize) -> Vec<AstNode> {
    let mut path = Vec::new();
    for expr in exprs {
        if expr.contains(cursor) {
            visit_expr(expr, cursor, &mut path);
            break;
        }
    }
    path
}

fn visit_constructor_name(name: &Spanned<Path>, path: &mut Vec<AstNode>) {
    path.push(AstNode {
        label: "ConstructorName",
        span: name.span,
    });
}

fn visit_block(block: &Spanned<Block>, cursor: usize, path: &mut Vec<AstNode>) {
    path.push(AstNode {
        label: "Block",
        span: block.span,
    });
    visit_block_raw(&block.data, cursor, path)
}
fn visit_block_raw(block: &Block, cursor: usize, path: &mut Vec<AstNode>) {
    for expr in &block.exprs {
        if expr.contains(cursor) {
            visit_expr(expr, cursor, path);
            return;
        }
    }
    if let Some(expr) = &block.value {
        if expr.contains(cursor) {
            visit_expr(expr, cursor, path);
        }
    }
}

fn visit_expr(expr: &Spanned<Expr>, cursor: usize, path: &mut Vec<AstNode>) {
    path.push(AstNode {
        label: expr_label(&expr.data),
        span: expr.span,
    });
    match &expr.data {
        Expr::Block(block) => visit_block_raw(block, cursor, path),
        Expr::Binding { expr, pattern } => {
            if expr.contains(cursor) {
                visit_expr(expr, cursor, path);
            } else if pattern.contains(cursor) {
                visit_pattern(pattern, cursor, path);
            }
        }
        Expr::ThenFlow {
            condition,
            then_block,
        } => {
            if condition.contains(cursor) {
                visit_expr(condition, cursor, path);
            } else if then_block.contains(cursor) {
                visit_block(then_block, cursor, path);
            }
        }
        Expr::ElseFlow {
            condition,
            else_block,
        } => {
            if condition.contains(cursor) {
                visit_expr(condition, cursor, path);
            } else if else_block.contains(cursor) {
                visit_block(else_block, cursor, path);
            }
        }
        Expr::ThenElseFlow {
            condition,
            then_block,
            else_block,
        } => {
            if condition.contains(cursor) {
                visit_expr(condition, cursor, path);
            } else if then_block.contains(cursor) {
                visit_block(then_block, cursor, path);
            } else if else_block.contains(cursor) {
                visit_block(else_block, cursor, path);
            }
        }
        Expr::Tuple { args } | Expr::Array { args } => {
            for arg in args {
                if arg.contains(cursor) {
                    visit_expr(arg, cursor, path);
                    break;
                }
            }
        }
        Expr::Constructor { name, args } => {
            if name.contains(cursor) {
                visit_constructor_name(name, path);
                return;
            }
            for arg in args {
                if arg.contains(cursor) {
                    visit_expr(arg, cursor, path);
                    break;
                }
            }
        }
        Expr::NamedConstructor { name, args } => {
            if name.contains(cursor) {
                visit_constructor_name(name, path);
                return;
            }
            for arg in args {
                if arg.contains(cursor) {
                    path.push(AstNode {
                        label: "NamedArg",
                        span: arg.span,
                    });
                    if let NamedExpr::Explicit { expr, .. } = &arg.data {
                        visit_expr(expr, cursor, path);
                    }
                    break;
                }
            }
        }
        Expr::True | Expr::False | Expr::Integer(_) | Expr::Identifier(_) | Expr::Path(_) => {}
    }
}

fn visit_pattern(pattern: &Spanned<Pattern>, cursor: usize, path: &mut Vec<AstNode>) {
    path.push(AstNode {
        label: pattern_label(&pattern.data),
        span: pattern.span,
    });
    match &pattern.data {
        Pattern::Alternatives { patterns }
        | Pattern::Tuple { patterns }
        | Pattern::Array { patterns } => {
            for pat in patterns {
                if pat.contains(cursor) {
                    visit_pattern(pat, cursor, path);
                    break;
                }
            }
        }
        Pattern::Constructor { name, patterns } => {
            if name.contains(cursor) {
                visit_constructor_name(name, path);
                return;
            }
            for pat in patterns {
                if pat.contains(cursor) {
                    visit_pattern(pat, cursor, path);
                    break;
                }
            }
        }
        Pattern::NamedConstructor { name, patterns } => {
            if name.contains(cursor) {
                visit_constructor_name(name, path);
                return;
            }
            for pat in patterns {
                if pat.contains(cursor) {
                    path.push(AstNode {
                        label: "NamedFieldPat",
                        span: pat.span,
                    });
                    if let NamedPattern::Explicit { pattern: inner, .. } = &pat.data {
                        if inner.contains(cursor) {
                            visit_pattern(inner, cursor, path);
                        }
                    }
                    break;
                }
            }
        }
        Pattern::MatchArms { arms } => {
            for (pat, block) in arms {
                let span = pat.span.merge(block.span);
                if span.contains(cursor) {
                    path.push(AstNode {
                        label: "MatchArm",
                        span,
                    });
                }
                if pat.contains(cursor) {
                    visit_pattern(pat, cursor, path);
                    break;
                }
                if block.contains(cursor) {
                    visit_block(block, cursor, path);
                    break;
                }
            }
        }
        Pattern::True
        | Pattern::False
        | Pattern::Wildcard
        | Pattern::Integer(_)
        | Pattern::Identifier(_)
        | Pattern::Path(_) => {}
    }
}

// ---------------------------------------------------------------------------
// Labels
// ---------------------------------------------------------------------------

fn expr_label(expr: &Expr) -> &'static str {
    match expr {
        Expr::True => "True",
        Expr::False => "False",
        Expr::Integer(_) => "Integer",
        Expr::Identifier(_) => "Identifier",
        Expr::Path(_) => "Path",
        Expr::Block(_) => "Block",
        Expr::Array { .. } => "Array",
        Expr::Tuple { .. } => "Tuple",
        Expr::Constructor { .. } => "Constructor",
        Expr::NamedConstructor { .. } => "NamedConstructor",
        Expr::Binding { .. } => "Binding",
        Expr::ThenFlow { .. } => "ThenFlow",
        Expr::ElseFlow { .. } => "ElseFlow",
        Expr::ThenElseFlow { .. } => "ThenElseFlow",
    }
}

fn pattern_label(pat: &Pattern) -> &'static str {
    match pat {
        Pattern::Wildcard => "Wildcard",
        Pattern::True => "TruePat",
        Pattern::False => "FalsePat",
        Pattern::Integer(_) => "IntegerPat",
        Pattern::Identifier(_) => "IdentifierPat",
        Pattern::Path(_) => "PathPat",
        Pattern::Array { .. } => "ArrayPat",
        Pattern::Tuple { .. } => "TuplePat",
        Pattern::Constructor { .. } => "ConstructorPat",
        Pattern::NamedConstructor { .. } => "NamedConstructorPat",
        Pattern::Alternatives { .. } => "Alternatives",
        Pattern::MatchArms { .. } => "MatchArms",
    }
}

// ---------------------------------------------------------------------------
// Error info
// ---------------------------------------------------------------------------

fn lex_error_info(error: &lexer::Error) -> (Span, String) {
    match error {
        lexer::Error::Unexpected(span) => (*span, "Unexpected character".into()),
    }
}

fn parser_error_info(error: &parser::Error) -> (Span, String) {
    match error {
        parser::Error::Lexer(le) => lex_error_info(le),
        parser::Error::Unexpected {
            message,
            expected,
            found,
            ..
        } => {
            let msg = if let Some(m) = message {
                if expected.len() > 0 {
                    format!("Expected {} ({:?}), found {:?}", m, expected[0], found.data)
                } else {
                    format!("Expected {}, found {:?}", m, found.data)
                }
            } else if expected.len() > 0 {
                format!("Expected {:?}, found {:?}", expected[0], found.data)
            } else {
                format!("Unexpected {:?}", found.data)
            };
            (found.span, msg)
        }
    }
}

// ---------------------------------------------------------------------------
// Utilities
// ---------------------------------------------------------------------------

fn pad(s: &str, width: usize) -> String {
    let count = s.chars().count();
    if count >= width {
        s.chars().take(width.saturating_sub(1)).collect::<String>() + "…"
    } else {
        format!("{:<width$}", s, width = width)
    }
}
