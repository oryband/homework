function rps(item1, item2) {
    // Simulates an rps game and sets the proper html element to indicate the
    // winner.
    'use strict';

    var result = (function (item1, item2) {
        // Behaves like strcmp(): return -1 for item1, 1 for item2,
        // and 0 for a tie.
        var choices = ['rock', 'paper', 'scissors'];

        // Error handling.
        if (item1 !== undefined && choices.indexOf(item1) === -1) {
            console.log('bad `item1` argument.');
            return null;
        }
        if (item2 !== undefined && choices.indexOf(item2) === -1) {
            console.log('bad `item2` argument.');
            return null;
        }

        // Generate random choices if not given as argument.
        item1 = item1 || choices[Math.random()*2|0];
        item2 = item2 || choices[Math.random()*2|0];

        if (item1 === item2) {  // Tie
            return 0;
            // Condition where item1 wins.
        } else if (item1 === 'rock' && item2 === 'scissors') {
            return -1;
        } else if (item1 === 'paper' && item2 === 'rock') {
            return -1;
        } else if (item1 === 'scissors' && item2 === 'paper') {
            return -1;
        } else {  // If nothing was caught than surely item2 wins.
            return 1;
        }
    })(item1, item2),

    // Output result.
    e = document.getElementById('result');

    if (result === null) {
        e.innerHTML = 'bad arguments, check console.';
    } else if (result === -1) {
        e.innerHTML = 'item1 won.';
    } else if (result === 0) {
        e.innerHTML = 'tie.';
    } else {  // === 2
        e.innerHTML = 'item2 won.';
    }
}
