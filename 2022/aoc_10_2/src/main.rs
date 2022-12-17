use std::io::BufRead;

#[derive(Debug)]
enum Inst {
    Addx(i32),
    WAddx,
    Noop,
}

fn main() {
    let args = std::io::stdin().lock().lines().flatten();
    let inst = args.flat_map(|x| {
        if &x == "noop" {
            vec![Inst::Noop].into_iter()
        } else {
            vec![
                Inst::WAddx,
                Inst::Addx(x.split_once(' ').unwrap().1.parse().unwrap()),
            ]
            .into_iter()
        }
    });
    let mut reg = 1;
    let crt: Vec<bool> = inst
        .enumerate()
        .take(6 * 40)
        .map(|(i, x)| (i + 1, x))
        .map(|(i, inst)| {
            let crt_pos = (i - 1) % 40;
            let pixel =
                reg == crt_pos as i32 || reg - 1 == crt_pos as i32 || reg + 1 == crt_pos as i32;
            match inst {
                Inst::Noop | Inst::WAddx => (),
                Inst::Addx(v) => {
                    reg += v as i32;
                }
            };
            pixel
        })
        .collect();

    crt.chunks(40).for_each(|x| {
        x.iter()
            .for_each(|x| print!("{}", if *x { '#' } else { '.' }));
        println!();
    })
}
