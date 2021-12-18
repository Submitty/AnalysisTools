import fs from 'fs';
import Parser from 'tree-sitter';
import CLanguage from 'tree-sitter-c';
import CppLanguage from 'tree-sitter-cpp';
import JavaLanguage from 'tree-sitter-java';
import PythonLanguage from 'tree-sitter-python';

const LANGUAGES: {[language: string]: any} = {
  c: CLanguage,
  cpp: CppLanguage,
  java: JavaLanguage,
  python: PythonLanguage,
};

export function parseFile(language: string, file: string): Parser.Tree {
  if (!Object.keys(LANGUAGES).includes(language)) {
    throw new Error(`Language ${language} is not supported`);
  }
  const parser = new Parser();
  parser.setLanguage(LANGUAGES[language]);
  return parser.parse(fs.readFileSync(file, 'utf8'));
}
