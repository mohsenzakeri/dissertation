%thesis.cls is based on report.cls (D. Brosius, August 2006) - These changes were made to comply with the requirements set forth in the University of Maryland, College Park, Thesis and Dissertation Style Guide, 2005-2006, Spring 2006 Edition for the submittal of Ph.D. and Masters dissertations and theses to the University of Maryland.


%%
%% This is file `report.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% classes.dtx  (with options: `report')
%% 
%% This is a generated file.
%% 
%% Copyright 1993 1994 1995 1996 1997 1998 1999
%% The LaTeX3 Project and any individual authors listed elsewhere
%% in this file.
%% 
%% This file is part of the LaTeX2e system.
%% ----------------------------------------
%% 
%% It may be distributed under the terms of the LaTeX Project Public
%% License, as described in lppl.txt in the base LaTeX distribution.
%% Either version 1.0 or, at your option, any later version.
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{thesis}
              [2006/04/26 v0.1]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% My commands begin
\newcommand{\Sla}{Selective-alignment\xspace}
\newcommand{\sla}{selective-alignment\xspace}
\newcommand{\ssla}{selective\xspace}
\newcommand{\qm}{quasi-mapping\xspace}
\newcommand{\Qm}{Quasi-mapping\xspace}
\newcommand{\pa}{pseudoalignment\xspace}
\newcommand{\eqclass}[1]{\ensuremath{\left[#1\right]}\xspace}
\newcommand{\eqlabel}[1]{\ensuremath{\text{\texttt{lab}}\left(#1\right)}\xspace}
\newcommand{\eqcount}[1]{\ensuremath{\text{\texttt{count}}\left(#1\right)}\xspace}
\newcommand{\kmer}{$k$-mer\xspace}
\newcommand{\kmers}{$k$-mers\xspace}
\newcommand{\Kmer}{$K$-mer\xspace}
\newcommand{\Kmers}{$K$-mers\xspace}
% \newcommand{\kmer}{$k$-mer\,\,}
% \newcommand{\kmers}{$k$-mers\,\,}
% \newcommand{\Kmer}{$K$-mer\,\,}
% \newcommand{\Kmers}{$K$-mers\,\,}
% \newcommand{\kmerNS}{$k$-mer\xspace}
% \newcommand{\kmersNS}{$k$-mers\xspace}
\newcommand{\dbg}{de Bruijn graph\xspace}
\newcommand{\dbgs}{de Bruijn graphs\xspace}
\newcommand{\cdbg}{compacted De Bruijn graph\xspace}
\newcommand{\dbgshort}{dBg\xspace}
\newcommand{\ccdbgshort}{ccdBg\xspace}
\newcommand{\ccdbg}{colored compacted de Bruijn graph\xspace}

\newcommand{\polyester}{\emph{Polyester}\xspace}
\newcommand{\sailfish}{\emph{Sailfish}\xspace}
\newcommand{\rapmap}{\emph{RapMap}\xspace}
\newcommand{\kallisto}{\emph{kallisto}\xspace}
\newcommand{\Kallisto}{Kallisto\xspace}
\newcommand{\rsem}{\emph{RSEM}\xspace}
\newcommand{\puffaligner}{\emph{PuffAligner}\xspace}
\newcommand{\pufferfish}{\emph{Pufferfish}\xspace}
\newcommand{\bwa}{\emph{BWA}\xspace}
%\newcommand{\st}{STAR\xspace}
\newcommand{\st}{\emph{STAR}\xspace}
\newcommand{\salmon}{\emph{Salmon}\xspace}
\newcommand{\debga}{\emph{deBGA}\xspace}
\newcommand{\Ddebga}{\emph{DeBGA}\xspace}
\newcommand{\brownie}{\emph{BrownieAligner}\xspace}
\newcommand{\mason}{\emph{Mason}\xspace}
\newcommand{\isoem}{\emph{IsoEM}\xspace}
\newcommand{\mmseq}{\emph{mmseq}\xspace}
\newcommand{\Mmseq}{\emph{Mmseq}\xspace}
\newcommand{\bitseq}{\emph{BitSeq}\xspace}
\newcommand{\tigar}{\emph{Tigar}\xspace}
\newcommand{\express}{\emph{eXpress}\xspace}
%\newcommand{\Express}{\emph{EXpress}\xspace}
\newcommand{\hera}{\emph{Hera}\xspace}
\newcommand{\fleximer}{\emph{Fleximer}\xspace}
\newcommand{\rnaskim}{\emph{RNAskim}\xspace}
\newcommand{\hisat}{\emph{HISAT 2}\xspace}
\newcommand{\STAR}{\emph{STAR}\xspace}
\newcommand{\bt}{\emph{Bowtie2}\xspace}
\newcommand{\btie}{\emph{Bowtie}\xspace}
\newcommand{\ksw}{\emph{KSW2}\xspace}
\newcommand{\kr}{\emph{Kraken2}\xspace}
\newcommand{\br}{\emph{Bracken}\xspace}
\newcommand{\cufflinks}{\emph{Cufflinks}\xspace}
\newcommand{\rsemsim}{\emph{RSEM-sim}\xspace}
\newcommand{\bgreat}{\emph{BGreat}\xspace}
\newcommand{\vari}{\emph{Vari}\xspace}
\newcommand{\cortex}{\emph{Cortex}\xspace}
\newcommand{\rainbowfish}{\emph{Rainbowfish}\xspace}
\newcommand{\mantis}{\emph{Mantis}\xspace}

\newcommand{\fragments}{\ensuremath{\mathcal{F}}\xspace}
\newcommand{\frag}[1]{\ensuremath{f_{#1}}\xspace}
\newcommand{\Frags}{\ensuremath{\mathcal{F}}\xspace}
\newcommand{\fraglen}[1]{\ensuremath{d_{#1}}}

\newcommand{\txp}[1]{\ensuremath{t_{#1}}\xspace}
\newcommand{\txps}{\ensuremath{\mathcal{T}}\xspace}
\newcommand{\txpfrac}[1]{\ensuremath{\tau_{#1}}\xspace}
\newcommand{\txpfracs}{\ensuremath{\bm{\tau}}\xspace}

\newcommand{\allkmers}{\ensuremath{\bm{\mathcal{K}}}\xspace}
\newcommand{\samplekmer}{\ensuremath{{\kappa}}\xspace}
\newcommand{\SuffixArray}{\ensuremath{\Lambda}\xspace}
\newcommand{\denovo}{\emph{de novo}\xspace}

\newcommand{\salu}{\emph{Salmon-U}\xspace}
\newcommand{\salrf}{\emph{Salmon-RF}\xspace}
\newcommand{\salfm}{\emph{Salmon-FM}\xspace}
\newcommand{\expressEM}{\emph{eXpress ($+50$)}\xspace}
\newcommand{\fm}{\emph{full model}\xspace}
\newcommand{\salmonu}{\emph{Salmon-U}\xspace}
\newcommand{\salmonrf}{\emph{Salmon-RF}\xspace}
\newcommand{\salmonfm}{\emph{Salmon-FM}\xspace}

\newcommand{\cb}{\emph{compatibility-based}\xspace}
\newcommand{\rangebased}{\emph{range-based}\xspace}
\newcommand{\rankbased}{\emph{rank-based}\xspace}
\newcommand{\nab}{non-alignment-based\xspace}

\newcommand{\length}[1]{\ensuremath{\ell_{#1}}}
\newcommand{\elength}[1]{\ensuremath{\tilde{\ell}_{#1}}}
\newcommand{\expected}[1]{\ensuremath{\mathbb{E}\left\{#1\right\}}\xspace}
\newcommand{\nfrac}[1]{\ensuremath{\eta_{#1}}\xspace}
\newcommand{\estnfrac}[1]{\ensuremath{\hat{\eta}_{#1}}\xspace}
\newcommand{\prior}[1]{\ensuremath{\alpha^{0}_{#1}}\xspace}
\newcommand{\mapset}[1]{\ensuremath{\Omega\left(#1\right)}}

\newcommand{\eqsize}[1]{\ensuremath{N^{#1}}}
\newcommand{\eqclasses}{\ensuremath{\bm{\mathcal{C}}}\xspace}

\newtheorem{theorem}{Theorem}

\newcommand{\makenote}[1]{\textcolor{red}{#1}\xspace}

\newcommand{\kslcp}{k-safe-LCP\xspace}
\newcommand{\kslcps}{k-safe-LCPs\xspace}
\newcommand{\NIP}[1]{\texttt{NIP}\ensuremath{\left(#1\right)}\xspace}
\newcommand{\hit}[1]{\texttt{hit}\ensuremath{\left(#1\right)}\xspace}
\newcommand{\hits}[1]{\texttt{hits}\ensuremath{\left(#1\right)}\xspace}
\newcommand{\interval}[2]{\ensuremath{\left[#1,#2\right)}\xspace}
\newcommand{\ival}[1]{\texttt{I}\ensuremath{\left(#1\right)}}
\newcommand{\ceil}[1]{\left\lceil #1 \right\rceil}
\newcommand{\cm}{co-mapping\xspace}
\newcommand{\intr}{\emph{intruder}\xspace}
\newcommand{\T}[1]{\texttt{T}\ensuremath{\left[#1\right]}\xspace}
\newcommand{\SA}[1]{\texttt{SA}\ensuremath{\left[#1\right]}\xspace}
\newcommand{\LCP}[2]{\texttt{LCP}\ensuremath{(#1, #2)}\xspace}
\newcommand{\SLP}[2]{\texttt{SLP}\ensuremath{(#1, #2)}\xspace}
\newcommand{\mem}{MEM\xspace}
\newcommand{\mems}{MEMs\xspace}
\newcommand{\unimem}{uni-MEM\xspace}
\newcommand{\Unimem}{Uni-MEM\xspace}
\newcommand{\unimems}{uni-MEMs\xspace}
\newcommand{\Unimems}{Uni-MEMs\xspace}
\newcommand{\unitig}{unitig\xspace}
\newcommand{\unitigs}{unitigs\xspace}
\newcommand{\uni}{Uni\xspace}

\newcommand{\eqc}{equivalent class\xspace}
\newcommand{\eqcs}{equivalent classes\xspace}
\newcommand{\Eqc}{Equivalent class\xspace}
\newcommand{\Eqcs}{Equivalent classes\xspace}
\newcommand{\gib}{Gibbs\xspace}
\newcommand{\gibbs}{Gibbs sampling\xspace}
\newcommand{\boots}{Bootstrap sampling\xspace}
\newcommand{\boot}{Bootstrap\xspace}
\newcommand{\aboots}{augmented Bootstrap sampling\xspace}
\newcommand{\aboot}{augmented Bootstrap\xspace}
\newcommand{\pboot}{positional Bootstrap\xspace}
\newcommand{\pboots}{positional Bootstrap sampling\xspace}
\newcommand{\Pboot}{Positional Bootstrap\xspace}
\newcommand{\Pboots}{Positional Bootstrap sampling\xspace}
\newcommand{\mle}{maximum likelihood estimation\xspace}
\newcommand{\mles}{maximum likelihood estimations\xspace}

\newcommand{\gene}[1]{\texttt{#1}\xspace}
\newcommand{\tup}[2]{\ensuremath{\langle #1, \txp{#2} \rangle}}

\newcommand{\ubold}{\fontseries{b}\selectfont}% <-- for bold fonts

% \newcommand\diag[4]{%
%  \multicolumn{1}{p{#2}|}{\hskip-\tabcolsep
%   $\vcenter{
%     \begin{tikzpicture}[baseline=0,anchor=south west,inner sep=#1]
%       \path[use as bounding box] (0,0) rectangle (#2+2\tabcolsep,\baselineskip);
%       \node[minimum width={#2+2\tabcolsep-\pgflinewidth},
%         minimum  height=\baselineskip+\extrarowheight-\pgflinewidth] (box) {};
%       \draw[line cap=round] (box.north west) -- (box.south east);
%       \node[anchor=south west] at (box.south west) {#3};
%       \node[anchor=north east] at (box.north east) {#4};
%     \end{tikzpicture}
%   }$\hskip-\tabcolsep
%  }
% }


\newcommand{\conf}[1]{\texttt{#1}\xspace}

\newif\ifcomments        % here's how we switch comments on and off
\commentstrue
\newcommand{\MMP}[1][]{
\ifthenelse{\equal{#1}{}}{\text{MMP}}{\text{MMP}\ensuremath{_#1}}\xspace
}
\newcommand{\MMSP}[1][]{
\ifthenelse{\equal{#1}{}}{\text{MMSP}}{\text{MMSP}\ensuremath{_#1}}\xspace
}

\usepackage{xparse}
\NewDocumentCommand\likelihood{mg}{%
    \ensuremath{\IfNoValueTF{#2}{%
        \mathcal{L}\left(#1\right)
        }{%
            \mathcal{L}\left(#1 \mid #2 \right)
        }
    }
}
\NewDocumentCommand\Prob{omg}{%
    \IfNoValueTF{#3}{%
        \IfNoValueTF{#1}{
            \ensuremath{\Pr\left(#2\right)}
        }{
            \ensuremath{\Pr_{\left(#1\right)}\left(#2\right)}
      }}{%
        \IfNoValueTF{#1}{
            \ensuremath{\Pr\left(#2 \mid #3 \right)}
      }{
          \ensuremath{\Pr_{\left( #1 \right)}\left(#2 \mid #3 \right)}
      }
    }
}
\NewDocumentCommand\latent{ggg}{%
    \ensuremath{\IfNoValueTF{#1}{%
        \bm{Z}\xspace}{%
        \IfNoValueTF{#2}{%
            \bm{z}_{#1}\xspace
        }{%
            z_{#1#2}\xspace
        }
    }
  }
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% My commands end

\newcommand\@ptsize{}
\newif\if@restonecol
\newif\if@titlepage
\@titlepagetrue
\newif\if@openright
\if@compatibility\else
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\DeclareOption{a5paper}
   {\setlength\paperheight {210mm}%
    \setlength\paperwidth  {148mm}}
\DeclareOption{b5paper}
   {\setlength\paperheight {250mm}%
    \setlength\paperwidth  {176mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{legalpaper}
   {\setlength\paperheight {14in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{executivepaper}
   {\setlength\paperheight {10.5in}%
    \setlength\paperwidth  {7.25in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
\fi
\if@compatibility
  \renewcommand\@ptsize{0}
\else
\DeclareOption{10pt}{\renewcommand\@ptsize{0}}
\fi
\DeclareOption{11pt}{\renewcommand\@ptsize{1}}
\DeclareOption{12pt}{\renewcommand\@ptsize{2}}
\if@compatibility\else
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\fi
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\if@compatibility\else
\DeclareOption{final}{\setlength\overfullrule{0pt}}
\fi
\DeclareOption{titlepage}{\@titlepagetrue}
\if@compatibility\else
\DeclareOption{notitlepage}{\@titlepagefalse}
\fi
\if@compatibility
\else
\DeclareOption{openright}{\@openrighttrue}
\DeclareOption{openany}{\@openrightfalse}
\fi
\if@compatibility\else
\DeclareOption{onecolumn}{\@twocolumnfalse}
\fi
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}
\DeclareOption{openbib}{%
  \AtEndOfPackage{%
   \renewcommand\@openbib@code{%
      \advance\leftmargin\bibindent
      \itemindent -\bibindent
      \listparindent \itemindent
      \parsep \z@
      }%
   \renewcommand\newblock{\par}}%
}
\ExecuteOptions{letterpaper,10pt,oneside,onecolumn,final,openany}
\ProcessOptions
\input{size1\@ptsize.clo}
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand\baselinestretch{}
\setlength\parskip{0\p@ \@plus \p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand\topfraction{.7}
\setcounter{bottomnumber}{1}
\renewcommand\bottomfraction{.3}
\setcounter{totalnumber}{3}
\renewcommand\textfraction{.2}
\renewcommand\floatpagefraction{.5}
\setcounter{dbltopnumber}{2}
\renewcommand\dbltopfraction{.7}
\renewcommand\dblfloatpagefraction{.5}
\if@twoside
  \def\ps@headings{%
      \let\@oddfoot\@empty\let\@evenfoot\@empty
      \def\@evenhead{\thepage\hfil\slshape\leftmark}%
      \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
      \let\@mkboth\markboth
    \def\chaptermark##1{%
      \markboth {\MakeUppercase{%
        \ifnum \c@secnumdepth >\m@ne
            \@chapapp\ \thechapter. \ %
        \fi
        ##1}}{}}%
    \def\sectionmark##1{%
      \markright {\MakeUppercase{%
        \ifnum \c@secnumdepth >\z@
          \thesection. \ %
        \fi
        ##1}}}}
\else
  \def\ps@headings{%
    \let\@oddfoot\@empty
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\markboth
    \def\chaptermark##1{%
      \markright {\MakeUppercase{%
        \ifnum \c@secnumdepth >\m@ne
            \@chapapp\ \thechapter. \ %
        \fi
        ##1}}}}
\fi
\def\ps@myheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\@gobbletwo
    \let\chaptermark\@gobble
    \let\sectionmark\@gobble
    }
  \if@titlepage
  \newcommand\maketitle{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \let \footnote \thanks
  \null\vfil
  \vskip 60\p@
  \begin{center}%
    {\normalsize \@title \par}%
    \vskip 3em%
    {\normalsize
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
      \vskip 1.5em%
    {\normalsize \@date \par}%       % Set date in \normalsize size.
  \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}
\else
\newcommand\maketitle{\par
  \begingroup
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
    \def\@makefnmark{\rlap{\@textsuperscript{\normalfont\@thefnmark}}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
            \hb@xt@1.8em{%
                \hss\@textsuperscript{\normalfont\@thefnmark}}##1}%
    \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
      \fi
    \else
%% \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \thispagestyle{plain}\@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}
\def\@maketitle{%
  \newpage
  \null
  \vskip 2em%
  \begin{center}%
  \let \footnote \thanks
    {\normalsize \@title \par}%
    \vskip 1.5em%
    {\normalsize
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
    \vskip 1em%
    {\normalsize \@date}%
  \end{center}%
  \par
  \vskip 1.5em}
\fi
\newcommand*\chaptermark[1]{}
\setcounter{secnumdepth}{3}
\newcounter {part}
\newcounter {chapter}
\newcounter {section}[chapter]
\newcounter {subsection}[section]
\newcounter {subsubsection}[subsection]
\newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph]
\renewcommand \thepart {\@Roman\c@part}
\renewcommand \thechapter {\@arabic\c@chapter}
\renewcommand \thesection {\thechapter.\@arabic\c@section}
\renewcommand \thesubsection {\thesection.\@arabic\c@subsection}
\renewcommand \thesubsubsection {\thesubsection .\@arabic\c@subsubsection}
\renewcommand \theparagraph {\thesubsubsection.\@arabic\c@paragraph}
\renewcommand \thesubparagraph {\theparagraph.\@arabic\c@subparagraph}
\newcommand\@chapapp{\chaptername}
\newcommand\part{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \thispagestyle{plain}%
  \if@twocolumn
    \onecolumn
    \@tempswatrue
  \else
    \@tempswafalse
  \fi
  \null\vfil
  \secdef\@part\@spart}

\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >-2\relax
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    \markboth{}{}%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \ifnum \c@secnumdepth >-2\relax
      \normalsize \partname~\thepart
       \par
       \vskip 20\p@
     \fi
     \normalsize  #2\par}%
    \@endpart}
\def\@spart#1{%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \normalsize  #1\par}%
    \@endpart}
\def\@endpart{\vfil\newpage
              \if@twoside
                \null
                \thispagestyle{empty}%
                \newpage
              \fi
              \if@tempswa
                \twocolumn
              \fi}
\newcommand\chapter{\if@openright\cleardoublepage\fi
                    \thispagestyle{plain}%
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter}
\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                         \refstepcounter{chapter}%
                         \typeout{\@chapapp\space\thechapter.}%
                         \addcontentsline{toc}{chapter}%
                                   {\protect\numberline{\thechapter}#1}%
                    \else
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}%
                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading
                    \fi}
\def\@makechapterhead#1{%
 \vspace*{20\p@}%
   {\parindent \z@ \raggedright \normalfont
    \ifnum \c@secnumdepth >\m@ne
        \large \@chapapp\space \thechapter 
        \par\nobreak
 %       \vskip 20\p@
    \fi
    \interlinepenalty\@M
    \large   #1\par\nobreak 
  %  \vskip 40\p@
  }}
\def\@schapter#1{\if@twocolumn
                   \@topnewpage[\@makeschapterhead{#1}]%
                 \else
                   \@makeschapterhead{#1}%
                   \@afterheading
                 \fi}
\def\@makeschapterhead#1{%
%  \vspace*{50\p@}%
  {\parindent \z@ \raggedright
    \normalfont
    \interlinepenalty\@M
%\normalsize #1\par\nobreak
    \large   #1\par\nobreak
 %   \vskip 40\p@
  }}
\newcommand\section{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
%{\normalfont\normalsize}}
                                   {\normalfont\large}}
\newcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     %{\normalfont\normalsize}}
                {\normalfont\large}}
\newcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     %{\normalfont\normalsize}}
                {\normalfont\large}}
\newcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {3.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize}}
\newcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize}}
\if@twocolumn
  \setlength\leftmargini  {2em}
\else
  \setlength\leftmargini  {2.5em}
\fi
\leftmargin  \leftmargini
\setlength\leftmarginii  {2.2em}
\setlength\leftmarginiii {1.87em}
\setlength\leftmarginiv  {1.7em}
\if@twocolumn
  \setlength\leftmarginv  {.5em}
  \setlength\leftmarginvi {.5em}
\else
  \setlength\leftmarginv  {1em}
  \setlength\leftmarginvi {1em}
\fi
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\renewcommand\theenumi{\@arabic\c@enumi}
\renewcommand\theenumii{\@alph\c@enumii}
\renewcommand\theenumiii{\@roman\c@enumiii}
\renewcommand\theenumiv{\@Alph\c@enumiv}
\newcommand\labelenumi{\theenumi.}
\newcommand\labelenumii{(\theenumii)}
\newcommand\labelenumiii{\theenumiii.}
\newcommand\labelenumiv{\theenumiv.}
\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}
\newcommand\labelitemi{\textbullet}
\newcommand\labelitemii{\normalfont \textendash}
\newcommand\labelitemiii{\textasteriskcentered}
\newcommand\labelitemiv{\textperiodcentered}
\newenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand*\descriptionlabel[1]{\hspace\labelsep
                                \normalfont #1}
\if@titlepage
  \newenvironment{abstract}{%
      \titlepage
      \null\vfil
      \@beginparpenalty\@lowpenalty
      \begin{center}%
         \abstractname
        \@endparpenalty\@M
      \end{center}}%
     {\par\vfil\null\endtitlepage}
\else
  \newenvironment{abstract}{%
      \if@twocolumn
        \section*{\abstractname}%
      \else
        \small
        \begin{center}%
          { \abstractname\vspace{-.5em}\vspace{\z@}}%
        \end{center}%
        \quotation
      \fi}
      {\if@twocolumn\else\endquotation\fi}
\fi
\newenvironment{verse}
               {\let\\\@centercr
                \list{}{\itemsep      \z@
                        \itemindent   -1.5em%
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \advance\leftmargin 1.5em}%
                \item\relax}
               {\endlist}
\newenvironment{quotation}
               {\list{}{\listparindent 1.5em%
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item\relax}
               {\endlist}
\newenvironment{quote}
               {\list{}{\rightmargin\leftmargin}%
                \item\relax}
               {\endlist}
\if@compatibility
\newenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      \setcounter{page}\z@
    }%
    {\if@restonecol\twocolumn \else \newpage \fi
    }
\else
\newenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      \setcounter{page}\@ne
    }%
    {\if@restonecol\twocolumn \else \newpage \fi
     \if@twoside\else
        \setcounter{page}\@ne
     \fi
    }
\fi
\newcommand\appendix{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \gdef\@chapapp{\appendixname}%
  \gdef\thechapter{\@Alph\c@chapter}}
\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}
\@addtoreset {equation}{chapter}
\renewcommand\theequation
  {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@equation}
\newcounter{figure}[chapter]
\renewcommand \thefigure
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename~\thefigure}
\newenvironment{figure}
               {\@float{figure}}
               {\end@float}
\newenvironment{figure*}
               {\@dblfloat{figure}}
               {\end@dblfloat}
\newcounter{table}[chapter]
\renewcommand \thetable
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename~\thetable}
\newenvironment{table}
               {\@float{table}}
               {\end@float}
\newenvironment{table*}
               {\@dblfloat{table}}
               {\end@dblfloat}
\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{10\p@}
\setlength\belowcaptionskip{0\p@}
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{#1: #2}%
  \ifdim \wd\@tempboxa >\hsize
    #1: #2\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}
\newcommand\@pnumwidth{1.55em}
\newcommand\@tocrmarg{2.55em}
\newcommand\@dotsep{4.5}
\setcounter{tocdepth}{2}
\newcommand\tableofcontents{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\contentsname
        \@mkboth{%
           \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}%
    \@starttoc{toc}%
    \if@restonecol\twocolumn\fi
    }
\newcommand*\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \addvspace{2.25em \@plus\p@}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \normalsize  #1\hfil \hb@xt@\@pnumwidth{\hss #2}}\par
       \nobreak
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}%
    \endgroup
  \fi}
\newcommand*\l@chapter[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \vskip 1.0em \@plus\p@
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode 
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
      \penalty\@highpenalty
    \endgroup
  \fi}
\newcommand*\l@section{\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand*\l@subsection{\@dottedtocline{2}{3.8em}{3.2em}}
\newcommand*\l@subsubsection{\@dottedtocline{2}{7.0em}{4.1em}}
\newcommand*\l@paragraph{\@dottedtocline{4}{10em}{5em}}
\newcommand*\l@subparagraph{\@dottedtocline{5}{12em}{6em}}
\newcommand\listoffigures{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listfigurename
      \@mkboth{\MakeUppercase\listfigurename}%
              {\MakeUppercase\listfigurename}}%
    \addcontentsline{toc}{chapter}{\normalsize List of Figures}
    \@starttoc{lof}%
    \if@restonecol\twocolumn\fi
    }
\newcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand\listoftables{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listtablename
      \@mkboth{%
          \MakeUppercase\listtablename}{\MakeUppercase\listtablename}}%
    \addcontentsline{toc}{chapter}{\normalsize List of Tables}
    \@starttoc{lot}%
    \if@restonecol\twocolumn\fi
    }
\let\l@table\l@figure
\newdimen\bibindent
\setlength\bibindent{1.5em}
\newenvironment{thebibliography}[1]
     {\chapter*{\bibname
        \@mkboth{\MakeUppercase\bibname}{\MakeUppercase\bibname}}%
    \addcontentsline{toc}{chapter}{\normalsize Bibliography}
\vskip 2em%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
\newcommand\newblock{\hskip .11em\@plus.33em\@minus.07em}
\let\@openbib@code\@empty
\newenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\@makeschapterhead{\indexname}]%
                \@mkboth{\MakeUppercase\indexname}%
                        {\MakeUppercase\indexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}
\newcommand\@idxitem{\par\hangindent 40\p@}
\newcommand\subitem{\@idxitem \hspace*{20\p@}}
\newcommand\subsubitem{\@idxitem \hspace*{30\p@}}
\newcommand\indexspace{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}
\renewcommand\footnoterule{%
  \kern-3\p@
  \hrule\@width.4\columnwidth
  \kern2.6\p@}
\@addtoreset{footnote}{chapter}
\newcommand\@makefntext[1]{%
    \parindent 1em%
    \noindent
    \hb@xt@1.8em{\hss\@makefnmark}#1}
\newcommand\contentsname{\large\rm\begin{center}Table of Contents\end{center}}
\newcommand\listfigurename{\large\rm\begin{center}List of Figures\end{center}}
\newcommand\listtablename{\large\rm\begin{center}List of Tables\end{center}}
\newcommand\bibname{\large\rm\begin{center}Bibliography\end{center}}
\newcommand\indexname{Index}
\newcommand\figurename{Figure}
\newcommand\tablename{Table}
\newcommand\partname{Part}
\newcommand\chaptername{Chapter}
\newcommand\appendixname{Appendix}
\newcommand\abstractname{Abstract}
\def\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagestyle{plain}
\pagenumbering{arabic}
\if@twoside
\else
  \raggedbottom
\fi
\if@twocolumn
  \twocolumn
  \sloppy
  \flushbottom
\else
  \onecolumn
\fi
\endinput
%%
%% End of file `report.cls'.
