// @removeComments: false
var Person = makeClass( 
   { 
       /** 
        This is just another way to define a constructor. 
        @constructs 
        @param {string} name The name of the person. 
        */ 
       initialize: function(name) { 
           this.name = name; 
       } /* trailing comment 1*/, 
   } 
);