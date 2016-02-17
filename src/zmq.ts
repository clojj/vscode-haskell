'use strict'

const zmq = require("zmq");

export class Messenger {

  socket;
  
  private promiseResolver;

  constructor(address: string) {
    this.socket = zmq.socket("req");
    this.socket.bind(address);
    console.log('CONNECTED: ' + address);

    var self = this;
    this.socket.on('message', function(response: any) {
      var utf8 = response.toString('utf-8');
      // console.log("received response from parser: " + utf8);
      // todo: JSON.stringify() lexing + parsing
      if (self.promiseResolver != null) {
        self.promiseResolver(utf8);
        self.promiseResolver = null;       
      }
    });

    // this.socket.on('message', function(response) {
    //   var utf8 = response.toString('utf-8');
    //   // console.log("response: " + utf8);
    //   console.log("received response from parser");
    //   // var ast = parser.parse(utf8, {addSourceForLeafs: true});
    //   console.log("result: " + utf8);
    //   // todo: JSON.stringify() lexing + parsing
    // });
  }

  // send(text: string) {
  //   console.log("sending: " + text);
  //   this.socket.send(text);
  // }

  execute(text: string): Promise<string> {
    // console.log("sending: " + text);

    const self = this;

    return new Promise(
      (resolve: (str: string) => void, reject: (str: string) => void) => {

        self.promiseResolver = resolve;
        self.socket.send(text);
      }
    );
  }
}