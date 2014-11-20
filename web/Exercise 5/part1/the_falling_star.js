var mongoose = require('mongoose'),
    db = mongoose.connect('mongodb://localhost/db');

// define the Participant schema. the reasoning is explained in the file 'modeling'
var Participant = mongoose.model('Participant', new mongoose.Schema({
  name: String, // full name
  age: Number,
  city: String,
  job: String, // job title
  pet: String, // favorite pet 
  food: String, // favorite food

  // a list of songs the participant will choose from
  songs: [{title: String, length: Number}], 

  // the day in the week the participant will go up singing at. 
  // 4-Wednesday, 7-Saturday
  singingDay: Number 
}));

// define the Show schema. the reasoning is explained in the file 'modeling'
var Show = mongoose.model('Show', new mongoose.Schema({
  num: Number, // the show index

  // array of participants in the specific show, each
  // having 3 fields of information.
  participants: [{
    // count of SMSes the participant received in the show
    sms: Number, 

    // a reference to the participant document
    participant: {type: mongoose.Schema.Types.ObjectId, ref: 'Participant'},

    // the song the participant will sing at this show
    song: {title: String, length: Number}
  }]
}));

// q1 - A list of participants performing on day 'day' 
//      4 for Wednesday, 7 for Saturday.
//      The result is a list of name, age and city for participants
//      that go up to sing on day 'day'.
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

// q2 - Get information for a specific participant by name.
//      The result is a specific participant document that goes by
//      the name 'name'.
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
//      The result is a list of participants name and age who like the given pet.
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
//      each kind of food. the result is a list of food types and the count of
//      participants liking that kind of food.
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

// q5 - Get the calculated time of all songs of a specific show.
//      The result is one object with a 'length' field which is
//      total amount of song lengths for a given show num.
exports.getTotalLengthOfShow = function (showNum, callback) {
  Show.aggregate(
    [
      {$match: {num: showNum}},
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

// q6 - Get top 3 participants for each show, sorted by amount of SMS they received.
//      the result is a list of shows, and an array of 3 participants for each of them
//      ordered by SMSes they received.
exports.getTop3PerShow = function (callback) {
  Show.aggregate(
    [
      {$unwind: '$participants'},
      {$sort: {'participants.sms': -1}},
      {$group: {_id: '$num', participants: {$push: '$participants'}}}
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
//      along with the song they will sing, which will be a song off their chosen songs
//      in the participant document.
exports.getListOfParticipantsForDays = function (callback) {
  Participant.aggregate(
    [
      {$sort: {'name': 1}},
      {$group: {
        _id: '$singingDay', 
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

// q8 - Get total count of SMS per show document. the result is a list of show num's
//      with a 'total_sms' field containing the amount of SMSes sent for that show.
exports.getTotalCountOfSMSPerShow = function (callback) {
  Show.aggregate(
    [
      {$unwind: '$participants'},
      {$group: {_id: '$num', total_sms: {$sum: '$participants.sms'}}}
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
