use std::collections::HashSet;


static DATA : &str = include_str!("../../../data/input3.txt");

fn main() {
    let data = parse_data(DATA);
    let result = part1(&data);
    println!("{}", result);
    let result2 = part2(DATA);
    println!("{}", result2);
}

fn parse_data<'a>(input : &'a str) -> Vec<(&'a [u8], &'a [u8])> {
    let lines = input.split_terminator("\n");
    let mut out = Vec::new();
    for l in lines {
        let (first, second) = l.split_at(l.len()/2);
        out.push((first.as_bytes(), second.as_bytes()));
    }
    out
}

fn priority(ch : u8) -> i32 {
    if ch < 97 {
        ch as i32 - 38
    } else {
        ch as i32 - 96
    }
}

fn common<'a>(packs : &mut impl Iterator<Item=&'a [u8]>) -> u8 {
    let mut left : HashSet<u8> = HashSet::from_iter(packs.next().unwrap().iter().map(|c| *c));
    for p in packs {
        let next = HashSet::from_iter(p.iter().map(|c| *c));
        left = left.intersection(&next).map(|c| *c).collect();
    }
    left.into_iter().next().unwrap()
}

fn part1(packs : &Vec<(&[u8], &[u8])>) -> i32 {
    packs.into_iter().map(|(a, b)| common(&mut [*a,*b].into_iter())).map(priority).sum()
}

fn part2(data : & str) -> i32 {
    let lines :Vec<_> = data.split_terminator("\n").map(str::as_bytes).collect();
    let groups :Vec<_>= lines.chunks(3).collect();
    groups.into_iter().map(|chunk| common(&mut chunk.into_iter().map(|x| *x))).map(priority).sum()
}

#[cfg(test)]
static TEST_DATA : &str = r#"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"#;

#[test]
fn test_part1() {
    let data = parse_data(TEST_DATA);
    let result = part1(&data);
    assert_eq!(result, 157);
}

#[test]
fn test_part2() {
    let result = part2(TEST_DATA);
    assert_eq!(result, 70)
}