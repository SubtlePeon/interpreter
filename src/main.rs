use std::env;
use std::fs;
use std::process::ExitCode;

use interpreter_starter_rust::*;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return 0.into();
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut errored = false;
            let scanner = lexer::Lexer::new(&file_contents);
            let tokens: Vec<_> = scanner.collect();
            for tok in tokens {
                match tok {
                    Ok(tok) => println!("{}", tok.conv(&file_contents)),
                    Err(err) => {
                        errored = true;
                        eprintln!("{}", err);
                    }
                }
            }

            if errored {
                65.into()
            } else {
                0.into()
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let scanner = lexer::Lexer::new(&file_contents);
            let parser = parser::Parser::from_lexer(scanner);

            _ = parser;

            0.into()
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            0.into()
        }
    }
}
