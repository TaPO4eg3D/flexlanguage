use std::io;
use std::io::prelude::*;

use crate::lexer::Lexer;
use crate::lexer::tokens::EOF;

const PROMPT: &str = ">> ";

pub fn start() {
    let mut buffer = String::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut buffer).unwrap();
        let mut chars = buffer.chars().collect();

        let mut lexer = Lexer::new(buffer.len(), &mut chars);

        loop {
            let tok = lexer.next_token();
            if tok.kind == EOF { break; }
            println!("{:?}", tok);
        }
        buffer.clear();
    }
}
