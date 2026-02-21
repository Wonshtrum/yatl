use std::fmt::Debug;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn merge(self, other: Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
    pub fn attach<T: Debug + Clone>(self, data: T) -> Spanned<T> {
        Spanned { data, span: self }
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T: Debug + Clone> {
    pub data: T,
    pub span: Span,
}

impl<T: Debug + Clone> std::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.data
    }
}

#[derive(Debug, Clone)]
pub struct Position {
    pub row: usize,
    pub col: usize,
    pub line_start: usize,
    pub line_end: usize,
}

pub struct Source {
    pub name: String,
    pub buffer: Vec<char>,
    pub nl_map: Vec<usize>,
}

impl std::ops::Deref for Source {
    type Target = [char];
    fn deref(&self) -> &[char] {
        &self.buffer
    }
}

impl Source {
    pub fn new(name: String, buffer: &str) -> Self {
        let mut nl_map = Vec::new();
        let buffer = buffer.chars().collect::<Vec<_>>();
        for (i, c) in buffer.iter().copied().enumerate() {
            if c == '\n' {
                nl_map.push(i);
            }
        }
        Self {
            name,
            buffer,
            nl_map,
        }
    }

    pub fn index_to_position(&self, index: usize) -> Position {
        let mut line_start = 0;
        let mut line_end = 0;
        let mut row = 1;
        for pos in &self.nl_map {
            line_end = *pos;
            if index < *pos {
                break;
            }
            row += 1;
            line_start = *pos + 1;
        }
        Position {
            row,
            col: index - line_start,
            line_start,
            line_end,
        }
    }

    pub fn substring(&self, start: usize, end: usize) -> String {
        self.buffer[start..end].iter().copied().collect()
    }

    pub fn print_span(&self, span: Span) {
        let pos = self.index_to_position(span.start);
        println!("   --> {}:{}:{}", self.name, pos.row, pos.col);
        println!("    |");
        println!(
            "{: <3} | {}",
            pos.row,
            self.substring(pos.line_start, pos.line_end)
        );
        println!(
            "    | {: >col$}{:^>len$}",
            "",
            "",
            col = pos.col,
            len = span.end - span.start
        );
    }
}
