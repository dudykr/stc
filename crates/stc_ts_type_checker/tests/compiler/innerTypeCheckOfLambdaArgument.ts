function takesCallback(callback: (n) =>any) {

}
 
takesCallback(
 
	function inner(n) {
                // this line should raise an error
                // otherwise, there's a bug in overload resolution / partial typechecking
		var k: string = 10; 
    }
);
