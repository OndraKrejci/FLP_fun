
import sys
import subprocess

from typing import List, Set, Tuple

def err_exit(message: str, code: int = 1) -> None:
	print(message, file=sys.stderr)
	sys.exit(code)

class Rule():
	def __init__(self, left: str, right: str) -> None:
		self.left = left
		self.right = right

	def __str__(self) -> str:
		return '%s->%s' % (self.left, self.right)

	def __hash__(self) -> int:
		return hash(str(self))

	def __eq__(self, __o: object) -> bool:
		if type(__o) != Rule:
			return False
		
		return (self.left == __o.left and self.right == __o.right)

class CFG():
	def __init__(self, nonterms: Set[str], terms: Set[str], start: str, rules: Set[Rule]) -> None:
		self.nonterms = nonterms
		self.terms = terms
		self.start = start
		self.rules = rules

	def __str__(self) -> str:
		out = ','.join(self.nonterms) + '\n'
		out += ','.join(self.terms) + '\n'
		out += self.start

		for rule in self.rules:
			out += '\n' + str(rule)

		return out

	def __eq__(self, __o: object) -> bool:
		if type(__o) != CFG:
			return False
		
		return (
			self.nonterms == __o.nonterms and
			self.terms == __o.terms and
			self.start == __o.start and
			self.rules == __o.rules
		)

def load_cfg(fpath: str) -> CFG:
	with open(fpath, 'r') as inputf:
		return parse_cfg(inputf.readlines())

def parse_cfg(lines: List[str]) -> CFG:
	if len(lines) < 4:
		err_exit('Grammar definition shorter than 4 lines')

	nonterms = parse_nonterms(lines[0])
	terms = parse_terms(lines[1])

	return CFG(
		nonterms,
		terms,
		parse_start(lines[2], nonterms),
		parse_rules(lines[3:], nonterms)
	)

def parse_nonterms(line: str) -> Set[str]:
	symbols = line.split(',')

	nonterms = set()
	for symbol in symbols:
		symbol = symbol.strip()
		nonterms.add(symbol)
	return nonterms

def parse_terms(line: str) -> Set[str]:
	symbols = line.split(',')

	terms = set()
	for symbol in symbols:
		symbol = symbol.strip()
		if symbol.islower() or symbol in ["+", "-", "*", "/", "(", ")", "[", "]"]:
			terms.add(symbol)
		else:
			err_exit('Invalid term [%s]' % symbol)
	return terms

def parse_start(line: str, nonterms: Set[str]) -> str:
	start = line.strip()
	if start in nonterms:
		return start
	else:
		err_exit('Start symbol [%s] not a nonterminal' % start)

def parse_rules(lines: List[str], nonterms: Set[str]) -> Set[Rule]:
	rules = set()
	for line in lines:
		line = line.strip()
		parts = line.split('->')

		if len(parts) != 2:
			err_exit('Invalid rule [%s]' % line)

		if parts[0] not in nonterms:
			err_exit('Unknown nonterminal symbol [%s] in rule [%s]' % (parts[0], line))
		
		rules.add(Rule(parts[0], parts[1]))

	return rules

def test(args: List[str], refpath: str) -> None:
	refcfg = load_cfg(refpath)
	command = ['../flp21-fun'] + args
	out, _ = run_proc(command)
	testcfg = parse_cfg(out.splitlines())

	desc = '(%s) (%s)' % (' '.join(command), refpath)

	if testcfg == refcfg:
		print('[PASS] ' + desc)
	else:
		print('[FAIL] ' + desc)
		print(testcfg)
		print()
		print(refcfg)

def run_tests() -> None:
	test(['-i', 'cfg-pr4_14.txt'], 'cfg-pr4_14.txt')
	test(['-1', 'cfg-pr4_14.txt'], 'cfg-pr4_14.out')
	test(['-2', 'cfg-pr4_17.txt'], 'cfg-pr4_17.out')
	test(['-2', 'cfg-cv4_8_12.txt'], 'cfg-cv4_8_12.out')

def run_proc(args: List[str]) -> Tuple[str, str]:
	proc = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
	return proc.communicate(timeout=2)

if __name__ == '__main__':
	run_tests()
