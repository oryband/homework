var options = ['paper', 'rock', 'scissors'];

function is_valid_rps(item) {
  return options.indexOf(item) != -1;
}

function random_rps() {
  return options[Math.floor(Math.random()*3)];
}

function rps(item1, item2) {
  if (!item1) {
    item1 = random_rps();
  } else if (!is_valid_rps(item1)) {
    // invalid rps item given.
    // blah blah innerHTML error
    return;
  }

  if (!item2) {
    item2 = random_rps();
  } else if (!is_valid_rps(item2)) {
    // invalid rps item given.
    // blah blah innerHTML error
    return;
  }

  // set the players class to the item name, 
  // which will set the right picture for their decision
  document.getElementById('player1').class = item1;
  document.getElementById('player1').innerHTML = item1;
  document.getElementById('player2').class = item2;
  document.getElementById('player2').innerHTML = item2;

  // if both players chose the same item, we have a tie
  if (item1 == item2) {
    document.getElementById('winner').innerHTML = 'There is a tie :(';
    return;
  }

  if ((item1 == 'rock' && item2 == 'scissors')
      || (item1 == 'paper' && item2 == 'rock')
      || (item1 == 'scissors' && item2 == 'paper')) {
    // player1 won. in any other case - player2 won. (we already checked for ties)
    document.getElementById('winner').innerHTML = 'Player 1 is victorious!';
    return;
  }

  document.getElementById('winner').innerHTML = 'Player 2 is victorious!';
}
