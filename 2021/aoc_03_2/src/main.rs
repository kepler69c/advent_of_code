use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut oxygen_vec: Vec<&String> = args.iter().collect();
    let mut co2_vec: Vec<&String> = args.iter().collect();

    let mut i_bit = 0;
    while oxygen_vec.len() > 1 || co2_vec.len() > 1 {
        if oxygen_vec.len() > 1 {
            let mut nb_0 = 0;
            let mut nb_1 = 0;
            for bits in oxygen_vec.iter() {
                let i_bits: Vec<char> = bits.chars().collect();
                if i_bits[i_bit] == '0' {
                    nb_0 += 1;
                }
                if i_bits[i_bit] == '1' {
                    nb_1 += 1;
                }
            }
            let most_bit = {
                if nb_1 >= nb_0 {
                    '1'
                } else {
                    '0'
                }
            };
            let mut new_oxygen_vec: Vec<&String> = Vec::new();
            for bits in oxygen_vec.iter() {
                let i_bits: Vec<char> = bits.chars().collect();
                if i_bits[i_bit] == most_bit {
                    new_oxygen_vec.push(bits);
                }
            }
            oxygen_vec = new_oxygen_vec.clone();
        }
        if co2_vec.len() > 1 {
            let mut nb_0 = 0;
            let mut nb_1 = 0;
            for bits in co2_vec.iter() {
                let i_bits: Vec<char> = bits.chars().collect();
                if i_bits[i_bit] == '0' {
                    nb_0 += 1;
                }
                if i_bits[i_bit] == '1' {
                    nb_1 += 1;
                }
            }
            let least_bit = {
                if nb_0 <= nb_1 {
                    '0'
                } else {
                    '1'
                }
            };
            let mut new_co2_vec: Vec<&String> = Vec::new();
            for bits in co2_vec.iter() {
                let i_bits: Vec<char> = bits.chars().collect();
                if i_bits[i_bit] == least_bit {
                    new_co2_vec.push(bits);
                }
            }
            co2_vec = new_co2_vec.clone();
        }
        i_bit += 1;
    }

    let mut oxygen = 0;
    let i_bits: Vec<char> = oxygen_vec[0].chars().collect();
    for i in 0..i_bits.len() {
        if i_bits[i] == '1' {
            oxygen += 1 << (i_bits.len() - i - 1);
        }
    }

    let mut co2 = 0;
    let i_bits: Vec<char> = co2_vec[0].chars().collect();
    for i in 0..i_bits.len() {
        if i_bits[i] == '1' {
            co2 += 1 << (i_bits.len() - i - 1);
        }
    }

    println!("{}", oxygen * co2);
}
