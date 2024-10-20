from abc import ABC, abstractmethod

class Element(ABC):

    @abstractmethod
    def render(self):
        pass

class Spacer(Element):
    def __init__(self, length=1):
        self._length = length

    @property
    def length(self):
        return self._length

    def render(self):
        return " " * self.length
    

    
class Line(Element):
    def __init__(self, length, symbol='-'):
        self._length = length
        self._symbol = symbol

    @property
    def length(self):
        return self._length
    
    @property
    def symbol(self):
        return self._symbol
    
    def render(self):
        return self.symbol * self.length
    
class Text(Element):
    def __init__(self, text):
        self._text = text

    @property
    def text(self):
        return self._text
    
    def render(self):
        return self.text
    
class FancyText(Text, Element):
    def __init__(self, text, symbol="="):
        super().__init__(text)

        self._symbol = symbol

    @property
    def symbol(self):
        return self._symbol
    
    def render(self):
       return self.symbol + self.symbol.join(self.text) + self.symbol
    
class HorizontalStack(Element):
    def __init__(self, *elements):
        self._elements = elements if elements is not None else []
        
    @property
    def elements(self):
        return self._elements
    
    def render(self):
        string = ""
        for element in self._elements:
            string += element.render()
        return string


class VerticalStack(Element):
    def __init__(self, *elements):
        self._elements = elements if elements is not None else []

        
    @property
    def elements(self):
        return self._elements
    
    def render(self):
        content = []
        for element in self.elements:
            content.append(element.render())
        return "\n".join(content)
            

class Box(VerticalStack, Element):
    def __init__(self, width, *elements):
        self._width = width
        border_line = HorizontalStack(Text("+"), Line(width - 2, symbol="="), Text("+"))
        super().__init__(border_line, *elements, border_line)


        

    @property
    def width(self):
        return self._width
    
    def render(self):
        lines = []
        for index, element in enumerate(self._elements):
            if index == 0 or index == len(self._elements) - 1:
                line = element.render()
            else:
                content = element.render()

                max_content_width = self.width - 2

                if len(content) > max_content_width:
                    content = content[:max_content_width]
                else:
                    content += ' ' * (max_content_width - len(content))
  
                line = "|" + content + "|"
            lines.append(line)

        return "\n".join(lines)
           



class ProgressBar(Element):
    def __init__(self, length, progress):
        self._length = length

        if(progress < 0 or progress > 1):
            progress = 0

        self._progress = progress

    @property
    def length(self):
        return self._length
    
    @property
    def progress(self):
        return self._progress
    

    def render(self):
        loadLine = Line(int(self._length * self._progress), '=')
        leftLine = Line(int(self._length - loadLine.length - 2))
        return "[" + loadLine.render() + leftLine.render() + "]"

ui = Box(19, 
         FancyText("WELCOME!"), 
         Spacer(), 
         Text("Loading packages:THIS SHOULD NOT BE SHOWN IN THE BOX"), 
         HorizontalStack( Line(3), Spacer(), Text("cowsay") ), 
         HorizontalStack( Line(3), Spacer(), Text("lolcat") ), 
         HorizontalStack( Line(3, symbol=">"), Spacer(), Text("whoami"), Text("...") ), 
         Spacer(), 
         HorizontalStack( Spacer(), ProgressBar(15, 0.4), Spacer() ) )

print(ui.render())