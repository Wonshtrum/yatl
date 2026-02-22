mod ast;
mod editor;
mod lexer;
mod parser;
mod source;

fn main() {
    let source = source::Source::new(
        "src/test".into(),
        "
        1 -> { Option::Some([x,y,2,]) => {1}, Option::None => {2;} } else {};
        1 => {} => {} => {};
        1 -> {
            { T::A { x, } => {}, T::B { x: 2, y: x } => {}, },
            { T::C ( x, ) },
        };
        get::a::b(1 -> x) => { true } else { false };
        map(Option::Some);
        Point { x: 1, y: 2, } -> p;
        false
    ",
    );
    // let mut parser = match parser::Parser::try_new(&source) {
    //     Err(error) => return error.pretty_print(&source),
    //     Ok(parser) => parser,
    // };
    // let exprs = match parser.parse() {
    //     Err(error) => return error.pretty_print(&source),
    //     Ok(exprs) => println!("{exprs:#?}"),
    // };
    let mut ed = editor::Editor::from_source(source);
    if let Err(e) = ed.run() {
        eprintln!("Editor error: {e}");
    }
}
