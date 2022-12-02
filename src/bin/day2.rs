

static DATA : &str = include_str!("../../data/input2.txt");

fn main() {
    let rounds = parse_data(DATA);
    let score = part1(&rounds);
    println!("{}", score);
    let score2 = part2(&rounds);
    println!("{}", score2);
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Shape {
    Rock = 1,
    Paper = 2,
    Scissors = 3
}

impl Shape {
    fn winner(&self) -> Shape {
        match self {
            Shape::Rock => Shape::Paper,
            Shape::Paper => Shape::Scissors,
            Shape::Scissors => Shape::Rock
        }
    }

    fn loser(&self) -> Shape {
        match self {
            Shape::Rock => Shape::Scissors,
            Shape::Paper => Shape::Rock,
            Shape::Scissors => Shape::Paper
        }
    }
}

fn parse_data(input : &str) -> Vec<(char, char)> {
    let mut out = Vec::new();
    let lines = input.split_terminator("\n");
    for l in lines {
        if let Some ((a, b)) = l.split_once(" ") {
            out.push((a.chars().next().unwrap(), b.chars().next().unwrap()));
        }
    }
    out
}

fn score(my : Shape, other : Shape) -> i32 {
    let diff = if my == other.winner() {
        6
    } else if my == other {
        3
    } else {
        0
    };
    my as i32 + diff
}

fn shape(ch : &char) -> Shape {
    match ch {
        'A' | 'X' => Shape::Rock,
        'B' | 'Y' => Shape::Paper,
        'C' | 'Z' => Shape::Scissors,
        _ => panic!("Unknown shape")
    }
}

fn shape2((play, action) : &(char, char)) -> (Shape, Shape) {
    let other = shape(play);
    let mine = match action {
        'X' => other.loser(),
        'Y' => other,
        'Z' => other.winner(),
        _ => panic!("Invalid input")
    };
    (other, mine)
}

fn part1(rounds : &Vec<(char, char)>) -> i32 {
    let shapes = rounds.iter().map(|(a,b)| (shape(a), shape(b)));
    shapes.map(|x| score(x.1, x.0)).sum()
}

fn part2(rounds : &Vec<(char, char)>) -> i32 {
    let shapes = rounds.iter().map(|t| (shape2(t)));
    shapes.map(|x| score(x.1, x.0)).sum()
}

#[cfg(test)]
static TEST_DATA :&str = r#"
A Y
B X
C Z
"#;

#[test]
fn test_part1() {
    let rounds = parse_data(TEST_DATA);
    let score = part1(&rounds);
    assert_eq!(score, 15);
}

#[test]
fn test_part2() {
    let rounds = parse_data(TEST_DATA);
    let score = part2(&rounds);
    assert_eq!(score, 12);
}