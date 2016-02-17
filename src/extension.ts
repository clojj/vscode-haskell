/// <reference path="../node_modules/rx/ts/rx.es6.d.ts" />
'use strict'

import * as vscode from 'vscode';
import * as decorator from './decorator';
import * as zmq from './zmq';
import * as Rx from 'rx';

import * as cp from 'child_process';

var EventEmitter = require('events');

export function activate(context: vscode.ExtensionContext) {

  // TODO
  // let childProcess = cp.spawn('/Users/jwin/VSCodeExtensions/vscode-haskell/haskell-src/ghc-engine/dist/build/hlpsj/hlpsj', [], {});
  // if (childProcess.pid) {
  //   console.log('vscode-haskell: child-process started');
  // }
        
  console.log('vscode-haskell: Extension activated');

  var activeEditor = vscode.window.activeTextEditor;
  var e = new EventEmitter();
  var decorer = new decorator.Decorator(activeEditor);
  var lexer = new zmq.Messenger('ipc:///tmp/lexer');
  // var parser = new zmq.Messenger('ipc:///tmp/parser');

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
      lexer.send('{{exit}}');
      // parser.send('{{exit}}');
    }
  }, null, context.subscriptions);

  vscode.workspace.onDidChangeTextDocument(event => {
    if (activeEditor && event.document === activeEditor.document) {
      var contentChanges: vscode.TextDocumentContentChangeEvent[] = event.contentChanges;
      e.emit('data', contentChanges[0].range.start);
    }
  }, null, context.subscriptions);

  // TODO tsc error
  // src/extension.ts: error TS2339: Property 'fromEvent' does not exist on type 'ObservableStatic'.
  
  // wrap EventEmitter
  var source = Rx.Observable.fromEvent(e, 'data', undefined).debounce(500 /* ms */); 

  var subscription = source.subscribe(
    function(from: vscode.Position) {
      console.log('offset: %d', from.character);
      // decorer.refreshDecorations(from);
      // parser.send(activeEditor.document.getText()); // todo: extract subscriber for document

      lexer.execute(activeEditor.document.getText())
        .then((st) => {
          console.log("Promise resolved:\n" + st);
          
          // todo: decorate !
          var result = JSON.parse(st);
          if (result.right != undefined) {
            console.log("first pos: " + result.right[0][1]);
            var from: vscode.Position = new vscode.Position(result.right[0][1][0], result.right[0][1][1]);
            var to: vscode.Position = new vscode.Position(result.right[0][1][2], result.right[0][1][3]);
            var range = new vscode.Range(from, to);
            testDecorations = [];
            testDecorations.push({ range: range, hoverMessage: null });
            activeEditor.setDecorations(testDecoration, testDecorations);
          }
        });
      // parser.execute(activeEditor.document.getText())
      //   .then((st) => {
      //     console.log("Promise resolved:\n" + st);
      //     // todo: decorate !
      //   });
    },
    function(err) {
      console.log('Error: ' + err);
    },
    function() {
      console.log('Completed');
    }
  );

  var testDecorations: vscode.DecorationOptions[] = [];

  var testDecoration: vscode.TextEditorDecorationType = vscode.window.createTextEditorDecorationType({
    cursor: 'crosshair',
    backgroundColor: 'rgba(255,0,0,0.3)'
  });

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

