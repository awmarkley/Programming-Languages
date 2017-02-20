=begin
Andrew Markley and Boniface Sindala
CS401
Assignment 2
=end

require 'strscan'

class Lexer


  @@reserved = {

      #Comment
      /(\/\/.(.)*)/ => '',

      #While
      /while/ => ' while ',


      /end/ => ' end ',

      #Do
      /do/ => ' do ',

      #If
      /if/ => ' if ',

      /then/ => ' then ',

      /else/ => ' else ',

      /not/ => ' not ',

      #Assign
      /(:=)/ => ' := ',

      #Plus
      /(\+)/ => ' + ',

      #Times
      /(\*)/ => ' * ',

      #Divide
      /\// => ' / ',

      #Minus
      /(\-)/=> ' - ',

      #End of line
      /(;)/ => ' ; ',

      #Equals
      /(==)/ => ' == ',


      #Less Than or Equal to
      /(<=)|(=<)/ => ' <= ',

      #Greater than or equal to
      /(>=)|(=>)/ => ' >= ',

      #Less Than
      /</ => ' < ',

      #Greater than
      />/ => ' > ',

      #TRUE
      /\W(true)\W/ => ' true ',

      #FALSE
      /\W(false)\W/ => ' false ',

      #Open Parens
      /\(/ => ' ( ',

      #Close Parens
      /\)/ => ' ) ',



      #End of File
      /$/ => ' EOF',

      /\n/ => 'NEWLINE'
  }

  @@addSpaces = {
      '+' => ' + ',

      'while' => ' while ',

      'if' => ' if ',

      'then' => ' then ',

      'end' => ' end ',

      'else' => ' else ',

      ':=' => ' := ',

      'do' => ' do ',

      '*' => ' * ',

      '//' => ' // ',

      '/' => ' / ',

      '-' => ' - ',

      ';' => ' ; ',

      '==' => ' == ',

      '<' => ' < ',

      '>' => ' > ',

      '<=' => ' <= ',

      '=<' => ' <= ',

      '>=' => ' >= ',

      '=>' => ' >= ',

      'true' => ' true ',

      'false' => ' false ',

      '(' => ' ( ',

      ')' => ' ) ',

      '\n' => ' NEWLINE '
  }

  @@symbolRegexp = {
      :integer => 888,
      :identifier => 999,
  }

  @@symbol = {

      ':=' => 0,
      '+' => 1,
      '-' => 2,
      '*' => 3,
      '/' => 4,

      'while' => 20,
      'if' => 21,
      'then' => 22,
      'else' => 23,
      'do' => 24,
      'end' => 25,

      'or' => 30,
      'and' => 31,
      'not' => 32,

      '==' => 50,
      '<' => 51,
      '>' => 52,
      '<=' => 53,
      '=<' => 54,
      '>=' => 55,
      '=>' => 56,

      'true' => 58,
      'false' => 59,

      '(' => 444,
      ')' => 555,

      ';' => 111,


      'EOF' => 9999,
  }

  #Original String
  @str

  #Original token text
  @split

  #Tokenized array
  @tokens

  #Backup tokenized array for searching
  @tokenBACKUP

  def initialize(input)
    @str = input
    splitstring(input)
    processTokens
  end

  def splitstring(input)
    preSplit = input.gsub!(Regexp.union(@@reserved.keys), @@addSpaces)

    @split = preSplit.gsub(/\s+/m, ' ').strip.split(' ') << 'EOF'
  end

  def processTokens()
    @tokens = @split.collect { |x| @@symbol[x] != nil ? @@symbol[x] : findValue(x)  }
    @tokenBACKUP = @tokens.dup
  end

  def findValue(value)
    case value
      when /[0-9]+/
        @@symbolRegexp[:integer]
      when /\w/
        @@symbolRegexp[:identifier]
      else
        '???'
    end

  end

  def peek()
    @tokens[0]
  end

  def getTokenText(index)
    @split[index]
  end

  def nextToken()
    @tokens.shift
  end

  def getOriginal(span)

    index = @tokenBACKUP.each_index.select{|x| span[0] == @tokenBACKUP[x] }

    index.each { |i|
      if @tokenBACKUP[i..i+span.length-1] == span
        return @split[i..i+span.length-1].join(' ')
      end
    }

  end

  def getReserved()
    @@reserved
  end

  def getAddSpaces()
    @@addSpaces
  end

  def getsplit()
    @split
  end

  def getSymbolRegexp()
    @@symbolRegexp
  end

end

=begin
lex = Lexer.new


str = <<-LINE
prev := 0;curr := 1;iter := 0;
while iter < N do // iterative fibonacci
tmp := prev + curr;
prev := curr ;
curr := tmp;
iter := iter + 1;
LINE

lex.splitstring(str)
puts lex.getsplit
lex.processTokens

puts '----'

puts lex.nextToken
puts lex.nextToken
=end