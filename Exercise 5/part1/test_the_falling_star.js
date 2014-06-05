var mongoose = require('mongoose'), 
    dbHelpers = require('./the_falling_star.js');

// q1
dbHelpers.getParticipantsOfDay(4, console.log);
dbHelpers.getParticipantsOfDay(7, console.log);

// q2
dbHelpers.getParticipantInformation('Mick Jagger', console.log);

// q3
dbHelpers.getParticipantsWhoLikePet('dogs', console.log);

// q4
dbHelpers.getParticipantsFavoriteFoodStats(console.log);

// q5
dbHelpers.getTotalLengthOfShow({_id: mongoose.Types.ObjectId('53909decf955e31b4a6b5033')}, console.log);

// q6
dbHelpers.getTop3PerShow(console.log);

// q7
dbHelpers.getListOfParticipantsForDays(function(result) { console.log(JSON.stringify(result)); });

// q8
dbHelpers.getTotalCountOfSMSPerShow(console.log);
