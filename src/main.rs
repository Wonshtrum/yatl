use std::io::{Read, Write};

mod ast;
mod editor;
mod lexer;
mod logger;
mod parser;
mod resolution;
mod source;

fn main() {
    let args = std::env::args();
    if args.len() > 2 {
        panic!("too many arguments");
    }

    let path = args.skip(1).next();
    let source = if let Some(path) = &path {
        let mut file = std::fs::File::open(&path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();
        source::Source::new(path.clone(), &content)
    } else {
        source::Source::empty("<empty>".into())
    };

    //let source = source::Source::empty("src/test".into());
    // let mut parser = match parser::Parser::try_new(&source) {
    //     Err(error) => return error.pretty_print(&source),
    //     Ok(parser) => parser,
    // };
    // let exprs = match parser.parse() {
    //     Err(error) => return error.pretty_print(&source),
    //     Ok(exprs) => println!("{exprs:#?}"),
    // };

    let mut editor = editor::Editor::from_source(source);
    if let Err(error) = editor.run() {
        eprintln!("Editor error: {error}");
    }

    let mut file = std::fs::File::create(path.as_deref().unwrap_or("out.rs")).unwrap();
    let content = editor.ctx.source.content();
    file.write_all(content.as_bytes()).unwrap();
}
