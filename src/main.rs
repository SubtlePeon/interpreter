use std::env;
use std::fs;
use std::io::{self, Write};
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
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut errored = false;
            let scanner = lexer::Lexer::new(&file_contents);
            let tokens: Vec<_> = scanner.collect();
            for tok in tokens {
                match tok {
                    Ok(tok) => println!("{}", tok),
                    Err(err) => {
                        errored = true;
                        eprintln!("{}", err);
                    },
                }
            }

            if errored {
                return 65.into();
            } else {
                return 0.into();
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            return 0.into();
        }
    }
}
