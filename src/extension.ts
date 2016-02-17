/// <reference path="../node_modules/rx/ts/rx.es6.d.ts" />
'use strict'

const EventEmitter = require('events');
const prettyHrtime = require('pretty-hrtime');
var R = require('ramda');

import * as vscode from 'vscode';
import * as decorator from './decorator';
import * as zmq from './zmq';
import * as Rx from 'rx';

import * as cp from 'child_process';

const eventEmitter = new EventEmitter();
var subscription;
const lexer = new zmq.Messenger('ipc:///tmp/lexer');
// const parser = new zmq.Messenger('ipc:///tmp/parser');
var childProcess;

export function deactivate(context: vscode.ExtensionContext) {
  // TODO disposals correct and complete ?
  console.log('vscode-haskell: stopping child-process...');
  subscription.dispose();
  lexer.send('{{exit}}');
  lexer.close();
  childProcess.disconnect();
  console.log('vscode-haskell: child-process killed');
}

export function activate(context: vscode.ExtensionContext) {

  // TODO use relative path
  childProcess = cp.spawn('/Users/jwin/VSCodeExtensions/vscode-haskell/haskell-src/ghc-engine/dist/build/hlsj/hlsj', [], {});
  if (childProcess.pid) {
    console.log('vscode-haskell: child-process started');
  }

  console.log('vscode-haskell: Extension activated');

  var activeEditor = vscode.window.activeTextEditor;
  const decorer = new decorator.Decorator(activeEditor);

  if (activeEditor) {
    // decorer.refreshDecorations(new vscode.Position(0, 0));
    // triggerUpdateDecorations(new vscode.Position(0, 0));
  }

  vscode.window.onDidChangeActiveTextEditor(editor => {
    console.log('onDidChangeActiveTextEditor');
    activeEditor = editor;
    decorer.activeEditor = activeEditor;
    if (editor) {
      // decorer.refreshDecorations(new vscode.Position(0, 0));
      // triggerUpdateDecorations(new vscode.Position(0, 0));
      
      // todo: move to disposal
      lexer.send('{{exit}}');
      // parser.send('{{exit}}');
    }
  }, null, context.subscriptions);

  vscode.workspace.onDidChangeTextDocument(event => {
    if (activeEditor && event.document === activeEditor.document) {
      var contentChanges: vscode.TextDocumentContentChangeEvent[] = event.contentChanges;
      // TODO also process multiple contentChanges (multi-cursor editing)
      eventEmitter.emit('data', contentChanges[0].range.start);
    }
  }, null, context.subscriptions);

  // TODO tsc error
  // src/extension.ts: error TS2339: Property 'fromEvent' does not exist on type 'ObservableStatic'.
  
  // wrap EventEmitter
  var source = Rx.Observable.fromEvent(eventEmitter, 'data').debounce(300 /* ms */); 

  // TODO extract subscriber for document
  subscription = source.subscribe(
    function(from: vscode.Position) {

      const start = process.hrtime();
      lexer.execute(activeEditor.document.getText()) // parser.execute(activeEditor.document.getText())
        .then((st) => {
          const end = process.hrtime(start);
          const dur = prettyHrtime(end);
          console.log("Promise resolved in " + dur + "\n" + st);
                                
          // todo: refactor to decorer, decorate correctly + optimally
          var result = JSON.parse(st);
          if (result.right != undefined) {
            // console.log("first pos: " + result.right[0][1]);
            testDecorations = [];
            R.forEach(element => {
              var from: vscode.Position = new vscode.Position(element[1][0], element[1][1]);
              var to: vscode.Position = new vscode.Position(element[1][2], element[1][3]);
              var range = new vscode.Range(from, to);
              testDecorations.push({ range: range, hoverMessage: null });
            }, result.right);
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

  // TODO remove obsolete debounce-fn
  // var timeout = null;
  // function triggerUpdateDecorations(from: vscode.Position) {
  //   console.log('triggerUpdateDecorations');
  //   decorer.refreshDecorations(from);
  //   if (timeout) {
  //     clearTimeout(timeout);
  //   }
  //   timeout = setTimeout(decorer.refreshDecorations(from), 500);
  // }

}

