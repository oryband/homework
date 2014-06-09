var mongoose = require('mongoose'), 
    dbHelpers = require('./the_falling_star.js');

function prettyLog(data) {
  console.log(JSON.stringify(data, undefined, 2));
}

// q1
dbHelpers.getParticipantsOfDay(4, prettyLog);
dbHelpers.getParticipantsOfDay(7, prettyLog);

// q2
dbHelpers.getParticipantInformation('Mick Jagger', prettyLog);

// q3
dbHelpers.getParticipantsWhoLikePet('dogs', prettyLog);

// q4
dbHelpers.getParticipantsFavoriteFoodStats(prettyLog);

// q5
dbHelpers.getTotalLengthOfShow(2, prettyLog);

// q6
dbHelpers.getTop3PerShow(prettyLog);

// q7
dbHelpers.getListOfParticipantsForDays(prettyLog);

// q8
dbHelpers.getTotalCountOfSMSPerShow(prettyLog);
