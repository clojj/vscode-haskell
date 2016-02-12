/// <reference path="../node_modules/rx/ts/rx.es6.d.ts" />
'use strict'

import * as vscode from 'vscode';
import * as decorator from './decorator';
import * as zmq from './zmq';
import * as Rx from 'rx';

var EventEmitter = require('events');

export function activate(context: vscode.ExtensionContext) {

  console.log('he: Extension activated');

  var activeEditor = vscode.window.activeTextEditor;
  var e = new EventEmitter();
  var decorer = new decorator.Decorator(activeEditor);
  // var lexer = new zmq.Messenger('ipc:///tmp/lexer');
  var parser = new zmq.Messenger('ipc:///tmp/parser');

  if (activeEditor) {
    decorer.refreshDecorations(new vscode.Position(0, 0));
    // triggerUpdateDecorations(new vscode.Position(0, 0));
  }

  vscode.window.onDidChangeActiveTextEditor(editor => {
    console.log('onDidChangeActiveTextEditor');
    activeEditor = editor;
    decorer.activeEditor = activeEditor;
    if (editor) {
      decorer.refreshDecorations(new vscode.Position(0, 0));
      // triggerUpdateDecorations(new vscode.Position(0, 0));
      
      // todo: move to disposal
      parser.send('{{exit}}');
    }
  }, null, context.subscriptions);

  vscode.workspace.onDidChangeTextDocument(event => {
    if (activeEditor && event.document === activeEditor.document) {
      var contentChanges: vscode.TextDocumentContentChangeEvent[] = event.contentChanges;
      e.emit('data', contentChanges[0].range.start);
    }
  }, null, context.subscriptions);

  // todo: tsc error
  // src/extension.ts: error TS2339: Property 'fromEvent' does not exist on type 'ObservableStatic'.
  
  // wrap EventEmitter
  var source = Rx.Observable.fromEvent(e, 'data', undefined);
    // .filter((from: vscode.Position, index: number, source: Rx.Observable<{}>) => from.character < 10);

  var subscription = source.subscribe(
    function(from: vscode.Position) {
      console.log('offset: %d', from.character);
      // decorer.refreshDecorations(from);
      // parser.send(activeEditor.document.getText()); // todo: extract subscriber for document

      parser.lexAndParse(activeEditor.document.getText())
        .then((st) => {
          console.log("Promise resolved:\n" + st);
          // todo: decorate !
        });
    },
    function(err) {
      console.log('Error: ' + err);
    },
    function() {
      console.log('Completed');
    }
  );


  var timeout = null;
  function triggerUpdateDecorations(from: vscode.Position) {
    console.log('triggerUpdateDecorations');
    decorer.refreshDecorations(from);
    if (timeout) {
      clearTimeout(timeout);
    }
    timeout = setTimeout(decorer.refreshDecorations(from), 500);
  }

}

