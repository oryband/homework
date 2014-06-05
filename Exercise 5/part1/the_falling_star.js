var mongoose = require('mongoose'),
    db = mongoose.connect('mongodb://localhost/db');

var Participant = mongoose.model('Participant', new mongoose.Schema({
  name: String,
  age: Number,
  city: String,
  job: String,
  pet: String,
  food: String,
  // TODO: fix after there's an answer for the issue with query 7
  //songs: [{type: mongoose.Schema.Types.ObjectId, ref: 'Song'}],
  songs: [{title: String, length: Number}],
  singingDay: Number
}));

var Show = mongoose.model('Show', new mongoose.Schema({
  participants: [{
    sms: Number,
    participant: {type: mongoose.Schema.Types.ObjectId, ref: 'Participant'},
    // TODO: fix after there's an answer for the issue with query 7
    //song: {type: mongoose.Schema.Types.ObjectId, ref: 'Song'}
    song: {title: String, length: Number}
  }]
}));

// TODO: fix after there's an answer for the issue with query 7
//var Song = mongoose.model('Song', new mongoose.Schema({
  //title: String,
  //length: Number
//}));

// q1 - A list of participants performing on day 'day' 
//      4 for Wednesday, 7 for Saturday
exports.getParticipantsOfDay = function (day, callback) {
  Participant.find({singingDay: day}, 'name age city').exec(function (err, result) {
    if (err) {
      console.error(err);
      return;
    }

    if (callback) {
      callback(result);
    }
  });
};

// q2 - Get information for a specific participant by name
exports.getParticipantInformation = function (name, callback) {
  Participant.find({name: name}).exec(function (err, result) {
    if (err) {
      console.error(err);
      return;
    }

    if (callback) {
      callback(result);
    }
  });
};

// q3 - Find all participants who like pets 'pet' and fetch their name and age.
exports.getParticipantsWhoLikePet = function (pet, callback) {
  Participant.find({pet: pet}, 'name age').exec(function (err, result) {
    if (err) {
      console.error(err);
      return;
    }

    if (callback) {
      callback(result);
    }
  });
};

// q4 - Get a list of all the favorite food and a count of how many people like 
//      each kind of food
exports.getParticipantsFavoriteFoodStats = function (callback) {
  Participant.aggregate(
    [
      {$group: {_id: '$food', count: {$sum: 1}}},
      {$project: {
        food: '$_id',
        count: '$count'
      }}
    ],
    function (err, result) {
      if (err) {
        console.error(err);
        return;
      }

      if (callback) {
        callback(result);
      }
    }
  );
};

// q5 - Get the calculated time of all songs of a specific show
exports.getTotalLengthOfShow = function (show, callback) {
  Show.aggregate(
    [
      {$match: {_id: show._id}},
      {$unwind: '$participants'},
      {$group: {_id: '', length: {$sum: '$participants.song.length'}}}
    ],
    function (err, result) {
      if (err) {
        console.error(err);
        return;
      }

      if (callback) {
        callback(result[0].length);
      }
    }
  );
};

// q6 - Get top 3 participants for each show
exports.getTop3PerShow = function (callback) {
  Show.aggregate(
    [
      {$unwind: '$participants'},
      {$sort: {'participants.sms': -1}},
      {$group: {_id: '$_id', participants: {$push: '$participants'}}}
    ],
    function (err, result) {
      if (err) {
        console.error(err);
        return;
      }

      // limit to 3 per show
      for (var i = 0; i < result.length; ++i) {
        result[i].participants.splice(3);
      }

      if (callback) {
        callback(result);
      }
    }
  );
};

// q7 - Retrieve a list of participants for each day of singing, sorted alphabatically
//      along with the song they will sing.
exports.getListOfParticipantsForDays = function (callback) {
  Participant.aggregate(
    [
      {$sort: {'name': 1}},
      {$group: {
        _id: '$singingDay', 
        // TODO: fix after there's an answer for the issue with the query
        participants: {$push: {name: '$name', song: '$songs[0]'}}
      }},
      {$project: {
        singingDay: '$_id',
        participants: '$participants'
      }}
    ],
    function (err, result) {
      if (err) {
        console.error(err);
        return;
      }

      if (callback) {
        callback(result);
      }
    }
  );
};

// q8 - Get total count of SMS per show 
exports.getTotalCountOfSMSPerShow = function (callback) {
  Show.aggregate(
    [
      {$unwind: '$participants'},
      {$group: {_id: '$_id', total_sms: {$sum: '$participants.sms'}}}
    ],
    function (err, result) {
      if (err) {
        console.error(err);
        return;
      }

      if (callback) {
        callback(result);
      }
    }
  );
};

//Participant.remove();
//Show.remove();

//var participant = new Participant();
//participant.name = 'Johnny Doe';
//participant.singingDay = 4;
//participant.age = 17;
//participant.city = 'Tel Aviv';
//participant.job = 'Software Engineer';
//participant.pet = 'dogs';
//participant.food = 'Mac n cheese';
//participant.save(function (err) {
  //if (err) {
    //console.log(err);
  //}
//});

//var participant2 = new Participant();
//participant2.name = 'Mick Jagger';
//participant2.singingDay = 4;
//participant2.age = 67;
//participant2.city = 'Tel Aviv';
//participant2.job = 'Software Engineer';
//participant2.pet = 'dogs';
//participant2.food = 'Mac n cheese';
//participant2.save(function (err) {
  //if (err) {
    //console.log(err);
  //}
//});

//var participant2 = new Participant();
//participant2.name = 'Donkey Kong';
//participant2.singingDay = 7;
//participant2.age = 45;
//participant2.city = 'Haifa';
//participant2.job = 'Singer';
//participant2.pet = 'cats';
//participant2.food = 'Pizza';
//participant2.save(function (err) {
  //if (err) {
    //console.log(err);
  //}
//});
//var show = new Show();
//show.participants.push({sms: 35, participant: participant2, song: {title: 'Something Blue', length: 24}});
//show.save(function (err) {
  //if (err) {
    //console.log(err);
  //}
//});

//Participant.find().exec(function (err, result) {
  //console.log('participants');
  //console.log(result);
//});

//Show.find().exec(function (err, result) {
  //console.log('shows');
  //console.log(result);
//});
