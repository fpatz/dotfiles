\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{fpbarticle}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass{article}
\RequirePackage{fontspec}
\usepackage[T1]{fontenc} % Use 8-bit encoding
\RequirePackage{mathpazo}

\usepackage{booktabs} % Required for better horizontal rules in tables
\usepackage{enumitem} % Required for list customisation
\setlist{noitemsep} % No spacing between list items

\usepackage{geometry} % Required for adjusting page dimensions and margins

\geometry{
  top=2.5cm, % Top margin
  bottom=3cm, % Bottom margin
  left=4cm, % Left margin
  right=4cm, % Right margin
  headheight=0.75cm, % Header height
  footskip=1.5cm, % Space from the bottom margin to the baseline of the footer
  headsep=0.75cm, % Space from the top margin to the baseline of the header
}

\usepackage[protrusion=true]{microtype} % Better typography
\usepackage{parskip}

\makeatletter
\renewcommand{\maketitle}{ % Customise the title (do not edit title and author name here)
  \vspace*{30pt} % Vertical whitespace before the title
  \begin{flushright} % Right align
    {\LARGE\@title} % Increase the font size of the title
    
    \vspace{50pt} % Vertical whitespace between the title and author name
    
    {\large\@author}\\ % Author name
    \@date % Date
    
    \vspace{40pt} % Vertical whitespace between the author block and abstract
  \end{flushright}
}
\makeatother

\usepackage{fancyhdr} % Required for customising headers and footers
\pagestyle{fancy} % Enable custom headers and footers
\renewcommand{\headrulewidth}{0pt} % Remove default top horizontal rule
\lhead{} % Left header
\chead{} % Centre header
\rhead{} % Right header
\lfoot{} % Left footer
\cfoot{\thepage} % Centre footer
\rfoot{} % Right footer

\RequirePackage[
  style=alphabetic,
  hyperref=true,
  backref=true,
  maxcitenames=3,
  url=true,
  backend=biber,
  natbib=true
]{biblatex}
\addbibresource{~/References/cslab.bib}
\usepackage{makeidx}
\makeindex
\RequirePackage{minted}
\usemintedstyle{algol_nu}
\usepackage{tocbibind}
\RequirePackage{hyperref}
\hypersetup{hidelinks}

\RequirePackage{csquotes}

\setmainfont[
  Extension=.otf,
  UprightFont=*-regular,
  ItalicFont=*-italic,
  BoldFont=*-bold,
  BoldItalicFont=*-bolditalic,
  Numbers=OldStyle,
]{texgyrepagella}

\newfontfamily\lnumpazo[
  Extension=.otf,
  UprightFont=*-regular,
  ItalicFont=*-italic,
  BoldFont=*-bold,
  BoldItalicFont=*-bolditalic,
  Numbers=Lining,
]{texgyrepagella}

\setmonofont[
Scale=0.8,
BoldFont=Fira Code Medium,
ItalicFont=Fira Code Light]{Fira Code Regular}

\usepackage{xcolor}
\definecolor{dropcapcolor}{HTML}{a52a2a}
\definecolor{sourcebg}{rgb}{0.92,0.92,0.92}

\usepackage{lettrine}
%\newfontfamily{\dropcapfont}[Color=dropcapcolor]{Typocaps}
%\renewcommand{\LettrineFontHook}{\dropcapfont}
\def\drop #1#2 {%
  \lettrine[lhang=0]{#1}{#2} % trailing space
}
\newenvironment{dropcap}{\drop}{\par}

\newenvironment{responsive}{}{}