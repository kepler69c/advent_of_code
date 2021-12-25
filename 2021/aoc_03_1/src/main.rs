use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let n_bits = args[0].len();
    let mut vec_n_bits = vec![0; n_bits];

    for bits in args.iter() {
        let i_bits: Vec<char> = bits.chars().collect();
        for i in 0..n_bits {
            if i_bits[i] == '1' {
                vec_n_bits[i] += 1;
            }
        }
    }

    let mut gamma = 0;
    let mut epsilon = 0;

    for i in 0..n_bits {
        if vec_n_bits[i] > args.len() / 2 {
            gamma += 1 << (n_bits - i - 1);
        } else {
            epsilon += 1 << (n_bits - i - 1);
        }
    }

    println!("{}", gamma * epsilon);
}
