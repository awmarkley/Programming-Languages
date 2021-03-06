=begin
Andrew Markley and Boniface Sindala
CS401
Assignment 2
=end

require_relative 'Lexer.rb'

class Analyzer

  @lexer

  def initialize(input)
    @lexer = Lexer.new(input)
  end

  def hasNextLine
    return false if @lexer.peek == 9999

    true
  end

  def getNextTokens
    currentLine = []

    loop do

      currentLine << @lexer.nextToken

      if currentLine[-1] == 9999
        puts 'ERROR: Unexpected EOF'
        return false
      end

      if currentLine[0] == 999
        break if currentLine[-1] == 111
      else
        break if (currentLine[-1] == 111 and currentLine[-2] == 25)
      end
    end

    currentLine[0..-2]
  end

  def getNextLine(query)

    currentLine = []

    loop do
      break if query.empty?

      currentLine << query.shift

      if currentLine[0] == 999
        break if currentLine[-1] == 111
      elsif currentLine[-1] == 9999
        unless currentLine[0] == 9999
          puts 'ERROR: unexpected EOF'
          return false
        else
          return true
        end
      else
        break if (currentLine[-1] == 111 and currentLine[-2] == 25)
      end
    end

    currentLine[0..-2]
  end

  def run
    if beginTest
         puts 'Passed!'
    else
      puts 'Failed!'
    end
  end

  def beginTest
    puts 'Analyzing...'
    while hasNextLine
      unless statement?(getNextTokens)
        return false
      end
    end

    true

  end

  def statements?(query)
    until query.empty?
      unless statement?(getNextLine(query))
        return false
      end
    end
    true
  end

  def statement?(query)
    if query == nil or query == [] or query == false
      return false
    end

    case query[0]
      #Assignment statement
      when 999
        if query[1] != 0
          puts "ERROR in #{@lexer.getOriginal(query)}: No assignment operator"
        end

        unless addop?(query[2..-1])
          puts "ERROR in #{@lexer.getOriginal(query)}"
          return false
        end


      #While statement
      when 20
        _do = query.find_index(24)

        if _do == nil
          puts 'ERROR in while statement. Missing keyword: do'
          puts @lexer.getOriginal(query)
          return false
        end

        unless lexp?(query[1.._do-1]) and statements?(query[_do+1..-2])
          puts "ERROR in while statement. #{@lexer.getOriginal(query)}"
          return false
        end


      #If Statement
      when 21
        _then = query.find_index(22)
        _else = query.find_index(23)

        if _then == nil or _else == nil
          puts 'Error in if statement. Missing keyword: then/else'
          puts @lexer.getOriginal(query)
          return false
        end

        unless lexp?(query[1.._then-1]) and statements?(query[_then+1.._else-1]) and statements?(query[_else+1..-1])
          puts 'Error in If statement.'
          puts @lexer.getOriginal(query)
          return false
        end
      else
        puts 'Error in statement'
        puts query
        puts @lexer.getOriginal(query)
        return false
    end

    true
  end

  def addop?(query)
    if query.size == 1
      return mulop?(query)
    end

    index = query.find_index(1)

    #Not addition, find subtraction
    if index == nil
      index = query.find_index(2)
    end

    if index == 0
      puts 'Addop ERROR ' + @lexer.getOriginal(query)
      return false
    end

    unless index == nil
      return (mulop?(query[0..index-1]) and mulop?(query[index+1..-1]))
    end

    puts 'Addop ERROR'
    false
  end

  def mulop?(query)
    if query.size == 1
      return factor?(query)
    end

    index = query.find_index(3)
    if index == nil
      index = query.find_index(4)
    end

    unless index == nil
      return (factor?(query[0..index-1]) and mulop?(query[index+1..-1]))
    end

    puts 'Mulop ERROR'
    false
  end

  def factor?(query)
    if query.size == 1 and (query[0] == 888 or query[0] == 999)
      return true
    end

    if query[0] == 444 and query[-1] == 555
      return addop?(query[1..-2])
    end

    false
  end

  def lexp?(query)
    if query.size == 1
      return lterm?(query)
    end

    _and = query.find_index(31)

    unless _and == nil
      return (lterm?(query[0.._and-1]) and lexp?(query[_and+1..-1]))
    else
      return (lterm?(query))
    end

    puts 'Lexp ERROR'
    false
  end

  def lterm?(query)
    if query.size == 1
      return lfactor?(query)
    end

    if query[0] == 32
      return lfactor?(query[1..-1])
    else
      return lfactor?(query)
    end

    puts 'Lterm ERROR'
    false
  end

  def lfactor?(query)
    if query == 58 or query == 59
      true
    elsif relop?(query)
      return true
    else
      puts "Lfactor ERROR"
      false
    end
  end

  def relop?(query)
    _comparator = query.find_index(53)

    if _comparator == nil
      _comparator = query.find_index(51)
    end

    if _comparator == nil
      _comparator = query.find_index(50)
    end

    unless _comparator == nil
      return (addop?(query[0.._comparator-1]) and addop?(query[_comparator+1..-1]))
    end

    puts "Relop ERROR in #{query}"
    false
  end
end

#
# str = <<-LINE
# prev := 0;curr := 1;iter := 0;
# while iter < N do // iterative fibonacci
# tmp := prev + curr;
# prev := curr ;
# curr := tmp;
# iter := iter + 1;
# end
# ;
# LINE
#
# strNope = <<-LINE
# prev := 0 curr := +1; iter := −0;
# / not a comment while N−− do
# tmp := prev (+ curr); prev = curr ;
# curr := ;
# end
# LINE
#
# puts 'Example 1 - Passing example!'
# puts
# puts str
# a = Analyzer.new str
# a.run
#
# puts
# puts '--------------'
# puts
#
#
# puts 'Example 2 - expected failure!'
# puts
# puts strNope
# b = Analyzer.new strNope
# b.run
#
# strTests = [
#     'prev := 0 curr := +1;',
#     'iter := -0;',
#     '/ not a comment while N−− do',
#     'tmp := prev (+ curr); prev = curr ;',
#     'curr := ;',
#     '//Just a file with comments'
# ]
#
# strTests.each { |x|
#   puts
#   puts '--------------'
#   puts
#   puts 'Testing ' + x
#   c = Analyzer.new x
#   c.run
# }
