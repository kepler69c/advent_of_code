use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut players_pos: Vec<_> = args
        .iter()
        .map(|x| {
            (1..=10)
                .cycle()
                .skip(x.split(' ').last().unwrap().parse::<usize>().unwrap())
        })
        .collect();

    let mut players_sco = vec![0; players_pos.len()];

    let mut dice = (1..=100).cycle();
    let mut turn = (0..players_pos.len()).cycle().peekable();
    let mut nb_dice = 0;

    while players_sco.iter().all(|&x| x < 1000) {
        let roll = dice.next().unwrap() + dice.next().unwrap() + dice.next().unwrap();
        nb_dice += 3;
        players_sco[*turn.peek().unwrap()] +=
            players_pos[*turn.peek().unwrap()].nth(roll - 1).unwrap();
        turn.next();

        println!("{:?}", (roll, &players_sco));
    }

    println!("{}", players_sco.iter().min().unwrap() * nb_dice);
}
