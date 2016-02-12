'use strict'

// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

export class Decorator {

  activeEditor: vscode.TextEditor;

  smallNumberDecorationType: vscode.TextEditorDecorationType;
  veryLargeNumberDecorationType: vscode.TextEditorDecorationType;
  largeNumberDecorationType: vscode.TextEditorDecorationType;

  regEx: RegExp;

  smallNumbers: vscode.DecorationOptions[] = [];
  largeNumbers: vscode.DecorationOptions[] = [];
  veryLargeNumbers: vscode.Range[] = [];

  constructor(editor: vscode.TextEditor) {
    this.activeEditor = editor;
    this.regEx = /\d+/g;
    
    this.smallNumberDecorationType = vscode.window.createTextEditorDecorationType({
      borderWidth: '1px',
      borderStyle: 'solid',
      overviewRulerColor: 'blue',
      overviewRulerLane: vscode.OverviewRulerLane.Right,
      borderColor: 'lightblue'
    });
    this.largeNumberDecorationType = vscode.window.createTextEditorDecorationType({
      cursor: 'crosshair',
      backgroundColor: 'rgba(255,0,0,0.3)'
    });
    this.veryLargeNumberDecorationType = vscode.window.createTextEditorDecorationType({
      backgroundColor: 'darkblue'
    });
  }

  /**
  *  Pushes decorations.
  *  @param {vscode.Position} [from] Position from which to start
  */
  refreshDecorations(from: vscode.Position) {
    // if (!this.activeEditor) {
    //   return;
    // }

    var text = this.activeEditor.document.getText(new vscode.Range(from, this.activeEditor.document.positionAt(this.activeEditor.document.getText().length)));
		
    var match;
    while (match = this.regEx.exec(text)) {
      let idx = match.index + this.activeEditor.document.offsetAt(from);
      let startPos = this.activeEditor.document.positionAt(idx);
      let endPos = this.activeEditor.document.positionAt(idx + match[0].length);

      var range = new vscode.Range(startPos, endPos);
      var decoration = { range: range, hoverMessage: 'Number **' + match[0] + '**' };
      if (match[0].length < 3) {
        this.smallNumbers.push(decoration);
      } else if (match[0].length < 5) {
        this.largeNumbers.push(decoration);
      } else {
        this.veryLargeNumbers.push(range);
      }


    }
    this.activeEditor.setDecorations(this.smallNumberDecorationType, this.smallNumbers);
    this.activeEditor.setDecorations(this.largeNumberDecorationType, this.largeNumbers);
    this.activeEditor.setDecorations(this.veryLargeNumberDecorationType, this.veryLargeNumbers);
  }
}