mod ast;
mod lexer;
mod parser;

fn main() {
    let mut lexer = lexer::Lexer::new(
        "src/test".into(),
        "
        1 -> { Option::Some([x,y,2,]) => {1} Option::None => {2;} } else {};
        1 => {} => {} => {};
        1 -> {
            { T::A { x, } => {}, T::B { x: 2, y: x } => {}, },
            { T::C ( x, ) },
        };
        get::a::b(1 -> x);
        map(Option::Some);
        Point { x: 1, y: 2, } -> p;
    ",
    );
    let tokens = lexer.tokenize();
    println!("{tokens:#?}");
    if let Err(error) = &tokens {
        lexer.pretty_print(error);
    }
    let mut parser = parser::Parser::from_tokens(lexer, tokens.unwrap());

    let exprs = parser.parse();
    println!("{exprs:#?}");
    if let Err(error) = &exprs {
        parser.pretty_print(error);
    }
}
