use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let i_args: Vec<u32> = args.iter().map(|x| x.parse::<u32>().unwrap()).collect();

    let mut counter = 0;
    for i in 0..i_args.len() - 1 {
        if i_args[i] < i_args[i + 1] {
            counter += 1;
        }
    }

    println!("{}", counter);
}
