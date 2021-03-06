var options = ['paper', 'rock', 'scissors'];

function is_valid_rps(item) {
  return options.indexOf(item) !== -1;
}

function random_rps() {
  return options[Math.floor(Math.random() * 3)];
}

function rps(item1, item2) {
  if (!item1) {
    item1 = random_rps();
  } else if (!is_valid_rps(item1)) {
    // invalid rps item given.
    document.getElementById('winner').innerHTML = 'ERROR: Wrong input given for player1 ("' + item1 + '")';
    return;
  }

  if (!item2) {
    item2 = random_rps();
  } else if (!is_valid_rps(item2)) {
    // invalid rps item given.
    document.getElementById('winner').innerHTML = 'ERROR: Wrong input given for player2 ("' + item2 + '")';
    return;
  }

  // set the players class to the item name, 
  // which will set the right picture for their decision
  document.getElementById('player1').className = item1;
  document.getElementById('player2').className = item2;

  // if both players chose the same item, we have a tie
  if (item1 === item2) {
    document.getElementById('winner').innerHTML = 'There is a tie :(';
    return;
  }

  if ((item1 === 'rock' && item2 === 'scissors')
      || (item1 === 'paper' && item2 === 'rock')
      || (item1 === 'scissors' && item2 === 'paper')) {
    // player1 won. in any other case - player2 won. (we already checked for ties)
    document.getElementById('winner').innerHTML = 'Player 1 is victorious!';
    return;
  }

  document.getElementById('winner').innerHTML = 'Player 2 is victorious!';
}
