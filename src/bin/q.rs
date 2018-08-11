#![feature(rustc_private)]

extern crate getopts;

use std::{env, result, process};
use getopts::{Options, Matches, ParsingStyle};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");
const VERSION_PRE: &'static str = env!("CARGO_PKG_VERSION_PRE");

struct Opts {
    version: bool
}

enum Error {
}

type Result<T> = result::Result<T, Error>;

fn usage_exit(options: &Options, fail: getopts::Fail) -> ! {
    println!("Error: {:?}", fail);
    println!("{}", options.usage("^.^"));
    process::exit(1);
}

fn try_version_exit() -> ! {
    println!("version {} {}", VERSION_PRE, VERSION);
    process::exit(1);
}

fn argparse() -> Matches {
    let args: Vec<String> = env::args().collect();

    let mut options = Options::new();
    options.parsing_style(ParsingStyle::FloatingFrees).long_only(true);

    options.optflag("v", "version", "program version");
    match options.parse(args) {
        Ok(matches) => matches,
        Err(fail) => usage_exit(&options, fail),
    }
}

fn main() {
    let matches = argparse();
    if matches.opt_present("version") {
        try_version_exit();
    }
}
