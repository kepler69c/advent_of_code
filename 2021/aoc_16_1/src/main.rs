use std::io::BufRead;

fn get_int_from_bits(m_iter: &mut std::slice::Iter<bool>, len: u32) -> u32 {
    let mut ret = 0;
    for i in 1..(len + 1) {
        ret += if *m_iter.next().unwrap() { 1 } else { 0 } << len - i;
    }
    ret
}

fn get_packet_info(m_iter: &mut std::slice::Iter<bool>) -> (u32, u32) {
    let ver = get_int_from_bits(m_iter, 3);
    let typ = get_int_from_bits(m_iter, 3);
    (ver, typ)
}

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let message: Vec<bool> = args[0]
        .chars()
        .map(|x| x.to_digit(16).unwrap())
        .map(|x| {
            [
                x & 0b1000 != 0,
                x & 0b0100 != 0,
                x & 0b0010 != 0,
                x & 0b0001 != 0,
            ]
        })
        .flatten()
        .collect();

    let mut m_iter = message.iter();
    let mut ver_sum = 0;
    while m_iter.clone().count() > 11 {
        let (ver, typ) = get_packet_info(&mut m_iter);
        ver_sum += ver;

        if typ != 4 {
            if *m_iter.next().unwrap() {
                m_iter.nth(10);
            } else {
                m_iter.nth(14);
            }
        } else {
            while *m_iter.next().unwrap() {
                m_iter.nth(3);
            }
            m_iter.nth(3);
        }
    }

    println!("{}", ver_sum);
}
