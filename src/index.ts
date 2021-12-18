import { program } from 'commander';
import { countToken } from './count';
import { parseFile } from './parser';


program.version('1.0.0');

program.command('count <language> <file> <token>')
  .action((language: string, file: string, token: string) => {
    const tree = parseFile(language, file)
    const count = countToken(token, tree);
    console.log(count);
  });

program.parse(process.argv);
