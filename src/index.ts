import { program } from 'commander';
import { Counter, countToken } from './count';
import { diagnostics } from './diagnostics';
import { parseFile } from './parser';

function errorHandler(message: string) {
  console.error(message);
  process.exit(1);
}

program
  .version('1.0.0');

program
  .command('count <language> <counter> <token> <file>')
  .action((language: string, counter: string, token: string, file: string) => {
    const tree = parseFile(language, file);
    if (!Object.values<string>(Counter).includes(counter)) {
      errorHandler(`Unknown counter: ${counter}`);
    }
    console.log(countToken(tree, Counter[counter], token));
  });

program
  .command('diagnostics <language> <file>')
  .action((language: string, file: string) => {
    const tree = parseFile(language, file);
    console.log(JSON.stringify({[file]: diagnostics(tree, file)}, null, 2));
  });

program.parse(process.argv);
