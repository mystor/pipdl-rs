/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

extern crate pipdl;

use std::fs::File;
use std::env::args;
use std::io::Read;

fn run() -> Result<(), Box<std::error::Error>> {
    let filename = args().skip(1).next().ok_or("Filename expected")?;
    let mut f = File::open(&filename)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    if let Err(e) = pipdl::parse(&s) {
        eprintln!("{}", e);
        return Err(Box::new(e));
    }
    Ok(())
}

fn main() {
    run().unwrap();
}
