use std::collections::HashMap;
use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let players_pos: Vec<usize> = args
        .iter()
        .map(|x| x.split(' ').last().unwrap().parse().unwrap())
        .collect();

    let mut multiverse: HashMap<(usize, usize), HashMap<(usize, usize), usize>> = HashMap::new();
    multiverse
        .entry((players_pos[0], players_pos[1]))
        .or_insert(HashMap::new())
        .insert((0, 0), 1);

    let mut wins = [0, 0];

    while multiverse.len() > 0 {
        // p1 turn
        let mut multiverse_new_p1 = HashMap::new();
        for (spaces, score_value) in multiverse.iter() {
            let (s1, s2) = spaces;
            let dices = [
                (s1 + 3 - 1) % 10 + 1,
                (s1 + 4 - 1) % 10 + 1,
                (s1 + 5 - 1) % 10 + 1,
                (s1 + 4 - 1) % 10 + 1,
                (s1 + 5 - 1) % 10 + 1,
                (s1 + 6 - 1) % 10 + 1,
                (s1 + 5 - 1) % 10 + 1,
                (s1 + 6 - 1) % 10 + 1,
                (s1 + 7 - 1) % 10 + 1,
                (s1 + 4 - 1) % 10 + 1,
                (s1 + 5 - 1) % 10 + 1,
                (s1 + 6 - 1) % 10 + 1,
                (s1 + 5 - 1) % 10 + 1,
                (s1 + 6 - 1) % 10 + 1,
                (s1 + 7 - 1) % 10 + 1,
                (s1 + 6 - 1) % 10 + 1,
                (s1 + 7 - 1) % 10 + 1,
                (s1 + 8 - 1) % 10 + 1,
                (s1 + 5 - 1) % 10 + 1,
                (s1 + 6 - 1) % 10 + 1,
                (s1 + 7 - 1) % 10 + 1,
                (s1 + 6 - 1) % 10 + 1,
                (s1 + 7 - 1) % 10 + 1,
                (s1 + 8 - 1) % 10 + 1,
                (s1 + 7 - 1) % 10 + 1,
                (s1 + 8 - 1) % 10 + 1,
                (s1 + 9 - 1) % 10 + 1,
            ];
            for new_space in dices {
                let new_scores = multiverse_new_p1
                    .entry((new_space, *s2))
                    .or_insert(HashMap::new());
                for (scores, univ) in score_value.iter() {
                    if *spaces == (1, 3) && *scores == (16, 6) {
                        println!("!!! {:?}", (scores, univ, dices));
                    }
                    let (sc1, sc2) = scores;
                    if sc1 + new_space < 21 {
                        *new_scores.entry((sc1 + new_space, *sc2)).or_insert(0) += univ;
                    } else {
                        wins[0] += univ;
                    }
                }
                if new_scores.len() == 0 {
                    multiverse_new_p1.remove_entry(&(new_space, *s2));
                }
            }
        }

        // p2 turn
        let mut multiverse_new_p2 = HashMap::new();
        for (spaces, score_value) in multiverse_new_p1.iter() {
            let (s1, s2) = spaces;
            let dices = [
                (s2 + 3 - 1) % 10 + 1,
                (s2 + 4 - 1) % 10 + 1,
                (s2 + 5 - 1) % 10 + 1,
                (s2 + 4 - 1) % 10 + 1,
                (s2 + 5 - 1) % 10 + 1,
                (s2 + 6 - 1) % 10 + 1,
                (s2 + 5 - 1) % 10 + 1,
                (s2 + 6 - 1) % 10 + 1,
                (s2 + 7 - 1) % 10 + 1,
                (s2 + 4 - 1) % 10 + 1,
                (s2 + 5 - 1) % 10 + 1,
                (s2 + 6 - 1) % 10 + 1,
                (s2 + 5 - 1) % 10 + 1,
                (s2 + 6 - 1) % 10 + 1,
                (s2 + 7 - 1) % 10 + 1,
                (s2 + 6 - 1) % 10 + 1,
                (s2 + 7 - 1) % 10 + 1,
                (s2 + 8 - 1) % 10 + 1,
                (s2 + 5 - 1) % 10 + 1,
                (s2 + 6 - 1) % 10 + 1,
                (s2 + 7 - 1) % 10 + 1,
                (s2 + 6 - 1) % 10 + 1,
                (s2 + 7 - 1) % 10 + 1,
                (s2 + 8 - 1) % 10 + 1,
                (s2 + 7 - 1) % 10 + 1,
                (s2 + 8 - 1) % 10 + 1,
                (s2 + 9 - 1) % 10 + 1,
            ];
            for new_space in dices {
                let new_scores = multiverse_new_p2
                    .entry((*s1, new_space))
                    .or_insert(HashMap::new());
                for (scores, univ) in score_value.iter() {
                    let (sc1, sc2) = scores;
                    if sc2 + new_space < 21 {
                        *new_scores.entry((*sc1, sc2 + new_space)).or_insert(0) += univ;
                    } else {
                        wins[1] += univ;
                    }
                }
                if new_scores.len() == 0 {
                    multiverse_new_p2.remove_entry(&(*s1, new_space));
                }
            }
        }

        multiverse = multiverse_new_p2;
    }

    println!("{}", wins.iter().max().unwrap());
}
