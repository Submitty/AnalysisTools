import type { Tree } from "tree-sitter";

export enum Counter {
  token = 'token',
  node = 'node',
  call = 'call',
  func = 'func',
  depth = 'depth',
}

export function countToken(tree: Tree, counter: Counter, token: string): number {
  let count = 0;

  const cursor = tree.rootNode.walk()
  while (true) {
    if (cursor.gotoFirstChild()) {
      if (cursor.nodeType === token) {
        count++;
      }
      continue
    }
    if (cursor.gotoNextSibling()) {
      if (cursor.nodeType === token) {
        count++;
      }
      continue
    }
    cursor.gotoParent();
    while (!cursor.gotoNextSibling()) {
      if (!cursor.gotoParent()) {
        break;
      }
    }
    if (cursor.currentNode === tree.rootNode) {
      break;
    }
  }
  return count;
}
