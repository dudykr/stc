//@module: commonjs
declare module http {
	export interface Server { openPort: number; }
}

interface server {
    (): http.Server;
    startTime: Date;
}
 
var x = 5;
var server = new Date();
export = server;
 
