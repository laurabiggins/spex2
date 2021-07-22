/*
var data = [
    {
        "name":       "Tiger Nixon",
        "position":   "System Architect",
        "salary":     "$3,120",
        "start_date": "2011/04/25",
        "office":     "Edinburgh",
        "extn":       "5421"
    },
    {
        "name":       "Garrett Winters",
        "position":   "Director",
        "salary":     "$5,300",
        "start_date": "2011/07/25",
        "office":     "Edinburgh",
        "extn":       "8422"
    }
]
*/

$(document).ready( function () {
    $('#table_id').DataTable();
} );

$('#example').DataTable( {
    ajax: {
        url: 'arrays.txt',
        dataSrc: 'data'
    },
    columns: [
        { data: 'name' },
        { data: 'position' },
        { data: 'salary' },
        { data: 'office' }
    ]
} );

/*$('#example').DataTable( {
    data: data,
    columns: [
        { data: 'name' },
        { data: 'position' },
        { data: 'salary' },
        { data: 'office' }
    ]
} );
*/

var add = function (a, b) {
    return a + b;
};

// create an object with properties and a method
var jedi = {
    name: "Yoda",
    age: 899,
    talk: function () { alert("another... Sky... walk..."); }
};

// can add properties afterwards, can overwrite too
jedi.lightsaber = "purple"  // same dot syntax to access value
