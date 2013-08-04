from pyparsing import Word, Keyword, operatorPrecedence, opAssoc, alphas


class formula:
  box  = 1
  dia  = 2
  conj = 3
  disj = 4
  imp  = 5
  var  = 6

  top_str = 'true'
  bot_str = 'false'
  box_str = 'box'
  dia_str = 'dia'
  conj_str = '&'
  disj_str = 'v'
  imp_str = '->'
  
  op_to_str = {box:box_str, dia:dia_str, conj:conj_str, disj:disj_str, imp:imp_str}
  str_to_op = {box_str:box, dia_str:dia, conj_str:conj, disj_str:disj, imp_str:imp}

  @staticmethod
  def parse_str(_str):
    _var = Word(alphas)

    _box  = Keyword(formula.op_to_str[formula.box])
    _dia  = Keyword(formula.op_to_str[formula.dia])
    _modal =  _box | _dia
    _conj = Keyword(formula.op_to_str[formula.conj])
    _disj = Keyword(formula.op_to_str[formula.disj])
    _imp  = Keyword(formula.op_to_str[formula.imp])

    _formula = operatorPrecedence(_var,
                [(_modal, 1, opAssoc.RIGHT),
                 (_conj, 2, opAssoc.LEFT),
                 (_disj, 2, opAssoc.LEFT),
                 (_imp, 2, opAssoc.RIGHT)])

    ast = _formula.parseString(_str)
    return formula(ast[0], 10)
   
  def __init__(self, ast, parent_op):
    self.ast = ast
    self.parent_op = parent_op
    if len(ast)==3:
      self.op  = formula.str_to_op[ast[1]]
      self.arg = [formula(ast[0], self.op), formula(ast[2], self.op)]
    elif len(ast)==2:
      self.op = formula.str_to_op[ast[0]]
      self.arg = [formula(ast[1], self.op)]
    elif len(ast)==1 and ast[0] not in formula.str_to_op.keys():
      self.op = formula.var
      self._str = ast[0]
    else:
      print 'ast error', len(self.ast)
      for each in self.ast:
        print each,
      print ''

  def __str__(self):
    if self.op == formula.var:
      _str = self._str
    else:
      if len(self.arg)==2:
        _str = str(self.arg[0]) + ' ' + formula.op_to_str[self.op] + ' ' + str(self.arg[1])
      else:
        _str = formula.op_to_str[self.op] + ' ' + str(self.arg[0])
      if self.op > self.parent_op:
        _str = '(' + _str + ')'

    return _str


def test_formula_parser():
  _str = "a & b v dia box c -> box d v (a v e -> b)"
  print _str
  print formula.parse_str(_str)

  _str = "a & b v dia box c -> box d v (a v e -> b)"
  print _str
  print formula.parse_str(_str)


test_formula_parser()
