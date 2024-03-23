import gleam/io
import simplifile
import gleam/list
import gleam/int
import gleam/string
import gleam/result
import gleam/order

pub type Hand {
  Hand(cards: List(Int), bet: Int, rating: Result(Rating, Nil))
}

pub type Rating {
  Five
  Four
  FullHouse
  Three
  TwoPair
  OnePair
  HighCard
}

fn rating_to_int(rating: Rating) -> Int {
  case rating {
    Five -> 6
    Four -> 5
    FullHouse -> 4
    Three -> 3
    TwoPair -> 2
    OnePair -> 1
    HighCard -> 0
  }
}

fn hand_rating_to_int(hand: Hand) -> Int {
  case hand.rating {
    Ok(rating) -> rating_to_int(rating)
    _ -> rating_to_int(get_rating(hand))
  }
}

fn parse_card(card: String) -> Int {
  case card {
    "A" -> 14
    "K" -> 13
    "Q" -> 12
    "J" -> 11
    "T" -> 10
    x -> {
      x
      |> int.parse
      |> result.unwrap(-1)
    }
  }
}

fn parse_hand(s: String) -> Hand {
  let split = string.split(s, " ")

  let cards = split
  |> list.first
  |> result.unwrap("")
  |> string.to_graphemes
  |> list.map(parse_card)

  let bet = split
  |> list.last
  |> result.unwrap("-1")
  |> int.parse
  |> result.unwrap(-1)

  Hand(cards: cards, bet: bet, rating: Error(Nil))
}

fn parse_input(in: String) -> List(Hand) {
  in
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> list.map(parse_hand)
}

fn count(lst: List(a), item: a) -> Int {
  case lst {
    [] -> 0
    [x, ..rest] -> {
      case x == item {
        True -> 1 + count(rest, item)
        False -> count(rest, item)
      }
    }
  }
}

fn get_joker_rating(hand: Hand) -> Rating {
  // assert hand.cards contains -1
  let set = list.unique(hand.cards)

  case list.length(set) {
    1 -> Five
    2 -> Five
    3 -> {
      // Four, FullHouse
      let x_count = set
      |> list.first
      |> result.unwrap(-1)
      |> count(hand.cards, _)

      let y_count = set
      |> list.at(1)
      |> result.unwrap(-1)
      |> count(hand.cards, _)

      let z_count = set
      |> list.last
      |> result.unwrap(-1)
      |> count(hand.cards, _)

      let joker_count = count(hand.cards, -2)

      case int.max(x_count, int.max(y_count, z_count)) {
        3 -> Four
        2 -> {
          case joker_count {
            2 -> Four
            1 -> FullHouse
            _ -> panic as "Invalid hand"
          }
        }
        _ -> panic as "Invalid hand"
      }
    }
    4 -> Three
    5 -> OnePair
    _ -> panic as "Invalid hand"
  }
}

fn get_rating(hand: Hand) -> Rating {
  case list.contains(hand.cards, -2) {
    True -> get_joker_rating(hand)
    False -> {
      let set = list.unique(hand.cards)

      case list.length(set) {
        1 -> Five
        2 -> {
          // FullHouse, Four
          let set_0 = set
          |> list.first
          |> result.unwrap(-1)
          |> count(hand.cards, _)

          let set_1 = set
          |> list.last
          |> result.unwrap(-1)
          |> count(hand.cards, _)

          case int.max(set_0, set_1) {
            4 -> Four
            3 -> FullHouse
            _ -> panic as "Invalid hand"
          }
        }
        3 -> {
          // Three, TwoPair
          let set_0 = set
          |> list.first
          |> result.unwrap(-1)
          |> count(hand.cards, _)

          let set_1 = set
          |> list.at(1)
          |> result.unwrap(-1)
          |> count(hand.cards, _)

          let set_2 = set
          |> list.last
          |> result.unwrap(-1)
          |> count(hand.cards, _)

          case int.max(set_0, int.max(set_1, set_2)) {
            3 -> Three
            2 -> TwoPair
            _ -> panic as "Invalid hand"
          }
        }
        4 -> OnePair
        5 -> HighCard
        _ -> panic as "Invalid hand"
      }
    }
  }
}

fn compare_cards(a: List(Int), b: List(Int)) -> order.Order {
  case list.length(a) == list.length(b) {
    False -> panic as "Invalid hand comparison"
    True -> {
      case a, b {
        [], [] -> order.Eq
        [x, ..], [y, ..] if x < y -> order.Lt
        [x, ..], [y, ..] if x > y -> order.Gt
        [_, ..rest_a], [_, ..rest_b] -> compare_cards(rest_a, rest_b)
        _, _ -> panic as "Invalid hand comparison"
      }
    }
  }
}

fn compare_hands(a: Hand, b: Hand) -> order.Order {
  case hand_rating_to_int(a), hand_rating_to_int(b) {
    x, y if x == y -> compare_cards(a.cards, b.cards)
    x, y if x > y -> order.Gt
    _, _ -> order.Lt
  }
}

fn get_total_winnings(hands: List(Hand)) -> Int {
  hands
  |> list.sort(compare_hands)
  |> get_total_winnings_helper(1, 0)
}

fn get_total_winnings_helper(hands: List(Hand), rank: Int, res: Int) -> Int {
  case hands {
    [] -> res
    [x, ..rest] -> get_total_winnings_helper(rest, rank + 1, res + { rank * x.bet })
  }
}

fn convert_to_joker(hand: Hand) -> Hand {
  let new_cards = hand.cards
  |> list.map(fn(x) {
    case x {
      11 -> -2
      x -> x
    }
  })

  Hand(..hand, cards: new_cards)
}

pub fn main() {
  let input = "input.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> parse_input

  let p1 = input
  |> get_total_winnings
  |> int.to_string

  io.println("-- Part 1 --")
  io.println("Total winnings: " <> p1)

  let p2 = input
  |> list.map(convert_to_joker)
  |> get_total_winnings
  |> int.to_string

  io.println("-- Part 2 --")
  io.println("Total winnings: " <> p2)
}
