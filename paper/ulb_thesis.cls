%% ULB_THESIS.CLS
%% (Adapted to the Universite Libre de Bruxelles, by Patrick Meyer <pmeyer@ulb.ac.be>)
%% Primary LaTeX2e Document Class for Caltech Ph.D. Thesis
%% Ling Li <ling@caltech.edu>
%%
%% This class is an extension of the {report} document class, v1.4b, that
%% formats the title page and chapters according to Caltech's thesis
%% regulations. It additionally adds the ability to use the Lucida Bright
%% font set and have the table of contents render properly, and
%% automatically includes the correct version of the graphicx graphics
%% package when used with LaTeX or pdfLaTeX. This class requires the
%% "setspace" and "graphicx" packages.
%%
%% The files "cit_logo.eps" and "cit_logo.pdf" are the Caltech logos for the
%% title page, and should be distributed with this class.
%%
%% This class supports all options of the {report} document class, plus the
%% following additional options:
%%
%% defended - causes the cover page to list a "Defended" date instead of a
%% "Submitted" date (the date itself is still set the same way it would
%% be in a standard report class document). The option "\ifthesisdefended"
%% allows the author to write conditional code based on whether or not
%% this option is specified.
%%
%% techreport - causes a local publication number (see "\pubnum", below) to
%% be printed on the cover page instead of the copyright year and defense
%% or submission date. (Note: a copyright page is required by Caltech for
%% Ph.D. theses. This page will not be included in a technical report.)
%%
%% doublespace - causes the thesis to be rendered double-spaced, for ease in
%% writing comments
%%
%% draftfoot - adds a draft footer to each page, as well as changing the
%% "Submitted" (or "Defended") line on the cover to indicate that the thesis
%% is a draft copy. The macro "\ifthesisdraft" allows the author to write
%% conditional code based on whether or not this option is specified.
%%
%% lucida - causes the document to be typeset with the Lucida Bright font
%% set, and modifies spacing in the table of contents appropriately. The
%% macro "\iflucidafont" allows the author to write conditional code based
%% on whether or not this option is specified. In order to use lists of
%% theorems with the ntheorem package and Lucida Bright fonts, the author
%% should use the "ntheorem-thesis" style included with this document class;
%% it will format the list of theorems similarly to the list of figures
%% format defined herein.
%%
%% preprint - causes a local publication number (see "\pubnum", below) to be
%% added to the upper right corner of the title page. The macro
%% "\ifthesispreprint" allows the author to write conditional code based on
%% whether or not this option is specified.
%%
%% In addition to these options, the following macros are available:
%%
%% \ifpdf - allows the author to write conditional code based on whether or
%% not pdftex is being used to typeset the thesis. This is potentially
%% important when importing graphics files, and can also be used to
%% determine whether or not to set PDF document attributes.
%%
%% \iftwoside - allows the author to detect whether two-sided layout was
%% selected (for the purpose of including extra blank pages and the like).
%%
%% The following macros are available with this class, in addition to the
%% standard LaTeX macros for the report class (the macro arguments indicate
%% the default values as specified in this class):
%%
%% \degreeaward{Doctor of Philosophy}                 Degree to be awarded
%% \university{California Institute of Technology}    Institution name
%% \address{Pasadena, California}                     Institution address
%% \unilogo{cit_logo}                                 Institution logo
%% \copyyear{\the\year}                               Year on diploma
%% \pubnum{}                                          Publication number
%%
%%
%% The command "\maketitle" will generate a title page and, if applicable, a
%% copyright page conforming to Caltech requirements, and including all the
%% above information. Note that the logo specified in "\unilogo" should be
%% provided as both an .eps and a .pdf file, for use with both regular LaTeX
%% and pdfLaTeX, and the file extension should not be specified.
%%
%% The environment "dedication" will generate a page with the dedication
%% text.
%%
%% The command "\extrachapter" will generate an unnumbered chapter heading,
%% and will produce a corresponding entry in the Table of Contents. This
%% command is used for the {abstract} and {acknowledgments} environments,
%% defined below, and can also be used for extra front matter content such
%% as lists of figures or theorems.
%%
%% The initial portion of the document, which usually includes the abstract,
%% acknowledgments, table of contents, and any lists of figures or other
%% items that may be necessary, is numberred in roman numerals, while the
%% body of the thesis is numbered in arabic numerals. The \maketitle command
%% automatically starts the copyright page and the roman numbering. The
%% author should place the command \mainmatter between the inital portion and
%% the main body of the thesis. Below is the recommended order for components
%% of the front matter:
%%
%%   \maketitle                  % title page and (if not a technical report)
%%                               % copyright page, required by Caltech
%%   \begin{dedication}          % optional dedication
%%     (body of dedication)
%%   \end{dedication}
%%   \begin{acknowledgments}     % optional acknowledgments
%%     (body of acknowledgments)
%%   \end{acknowledgments}
%%   \begin{abstract}
%%     (body of abstract)        % abstract, required by Caltech
%%   \end{abstract}
%%   (tables of contents, figures, other front matter)
%%   \mainmatter
%%
%% The {abstract} and {acknowledgments} environments generate unnumbered
%% chapters that are listed in the table of contents.
%%
%% This class is adapted from Daniel M. Zimmerman's new_cit_thesis.cls,
%% ver 1.2, which was adapted from Michael H. Kelsey's cit_thesis.cls,
%% from February 1995. This class does not work with LaTeX 2.09.
%%

\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{ulb_thesis}
              [2006/06/20 v1.3.3
 Caltech thesis class]

% Define our new \if's
\newif\ifdoublespace
\newif\ifthesisdefended
\newif\iftechreport
\newif\ifthesisdraft
\newif\ifthesispreprint
\newif\iflucidafont
% \newif\ifpdf

% Set up \ifpdf
% \pdffalse       % no pdfLaTeX
% \ifx\pdfoutput\undefined
% \else
%  \ifx\pdfoutput\relax
%  \else
%    \pdftrue    % pdfLaTeX
%  \fi
% \fi
\usepackage{ifpdf}

% We require some other packages...
\RequirePackage{setspace}
\RequirePackage{graphicx}

% Our own options
\DeclareOption{defended}{\thesisdefendedtrue}
\DeclareOption{techreport}{\techreporttrue}
\DeclareOption{draftfoot}{\thesisdraftfalse}
\DeclareOption{preprint}{\thesispreprinttrue}
\DeclareOption{lucida}{\lucidafonttrue}
\DeclareOption{doublespace}{\doublespacetrue}

% Load report.cls
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\ProcessOptions\relax
\LoadClass[a4paper]{report}[2000/05/19]

\let\iftwoside\if@twoside

% Construct timestamp information for draft-mode footer
\newcount\timehh\newcount\timemm
\newcommand{\thesistime}{%
  \timehh=\time\divide\timehh by 60%
  \timemm=\timehh\multiply\timemm by -60%
  \advance\timemm by \time%
  \ifnum\timehh<10 0\fi\number\timehh\,:\,%
  \ifnum\timemm<10 0\fi\number\timemm%
  \ \ \today}

% We modify the headers and footers for Caltech requirements...
\newcommand{\@draftfoot}{DRAFT---NOT FOR DISTRIBUTION\hfil\thesistime}
\newcommand{\thesis@ps}{
  \def\@oddhead{\hss\thepage\hss}
  \ifthesisdraft
    \let\@oddfoot\@draftfoot
  \else
    \let\@oddfoot\@empty
  \fi
  \let\@evenhead\@oddhead \let\@evenfoot\@oddfoot}

\let\thesis@headings\ps@headings
\def\ps@headings{\thesis@headings\thesis@ps}

\let\thesis@myheadings\ps@myheadings
\def\ps@myheadings{\thesis@myheadings\thesis@ps}

\thesis@ps

\setcounter{secnumdepth}{3}
%
% Chapter headings at top of page, single line format
%
\def\@makechapterhead#1{\vspace*{50pt}
  {\parindent 0pt \raggedright \setstretch{1} \huge\bf%
   \ifnum \c@secnumdepth>\m@ne%
     \@chapapp{} \thechapter
     \par
     \vskip 20pt
   \fi
   \Huge \bf
   #1\par \nobreak \vskip 40pt  \setstretch{1.5}}
}               %% End of \@makechapterhead

% Chapter headings at top of page, single line format
\def\@makeschapterhead#1{\vspace*{50pt}
  {\parindent 0pt \raggedright \setstretch{1} \Huge\bf%
   #1\par \nobreak \vskip 40pt\setstretch{1.5} }
}               %% End of \@makeschapterhead

% Add a command to create an extra chapter with TOC entry.
\newcommand{\extrachapter}[2][\@empty]{%
  \chapter*{#2}%
  \addcontentsline{toc}{chapter}{\ifx#1\@empty{#2}\else{#1}\fi}}

% Table of contents parameters need to be changed, if we're using Lucida
\iflucidafont
  \renewcommand\@pnumwidth{2.00em}
  \renewcommand\@tocrmarg{2.75em}
\fi

% TOC depth is 3
\setcounter{tocdepth}{1}

% Contents lines for figures/tables aren't indented (support long numbers)
\renewcommand*\l@figure{\@dottedtocline{-2}{0em}{3.2em}}
\renewcommand*\l@table{\@dottedtocline{-2}{0em}{3.2em}}

% Define \thispagestyle to null so that \chapter doesn't change page style.
\AtBeginDocument{\thispagestyle{empty}\renewcommand{\thispagestyle}[1]{}}

% Text Spacing
\ifdoublespace
  \setstretch{1.7}
\else
  \setstretch{1.2}
\fi
%
\setlength{\oddsidemargin}{0.5cm}    %% Left margin 
\setlength{\evensidemargin}{0cm}    %% Right margin
\setlength{\textwidth}{15cm}         %% Makes right margin 
\setlength{\topmargin}{-0.5cm}       %% Top margin = 3/4'' inch (incl pg#)
\setlength{\headsep}{1cm}         %% Space between page # and text
\setlength{\textheight}{21.5cm}     %% Makes bottom margin = 1 inch
\setlength{\footskip}{.4in}         %% Space from bottom of text to foot
%
% Define spacing for use on titlepage
%
\def\titleskip{\vskip 4\bigskipamount}      %% Whitespace after title
\def\authorskip{\vskip 2\bigskipamount}     %% Whitespace after author
%
% We want roman page numbers in the frontmatter, and arabic afterward.
% use \mainmatter to separate them.
%
\newcommand{\mainmatter}{\clearpage\pagenumbering{arabic}}

%
% Additional titlepage definitions
%
\newcommand{\degreeaward}{\gdef\@degreeaward}
\degreeaward{Docteur}                

\newcommand{\university}{\gdef\@university}
\university{de l'Universit\'{e} Libre de Bruxelles}     

\newcommand{\unilogo}{\gdef\@unilogo}
\unilogo{sceau_a_quadri}                                

\newcommand{\address}{\gdef\@address}
\address{Bruxelles, Belgique}                    

\newcommand{\copyyear}{\gdef\@copyyear}
\copyyear{\the\year}                                % Default is today

\newcommand{\pubnum}{\gdef\@pubnum}
\pubnum{}                                           % No publication number
%
% The copyright page is new, must conform to CIT requirements
%
\newcommand{\makecopyright}{%
 {\parindent=0pt
 \null              % Forces rest to bottom of page
 \vfill
 \begin{center}
   \copyright~\@copyyear\\
   \@author\\
   All Rights Reserved
 \end{center}}
 \newpage
}               %% End of \makecopyright
%
% The titlepage must conform to CIT requirements
%
\def\maketitle{%
 {\parindent=5pt
 \begin{titlepage}
   \let\footnotesize\small \let\footnoterule\relax \setcounter{page}{1}
   \vskip-\headsep
   \ifthesispreprint\ifx\empty\@pubnum\else
      {\singlespace\hfill\hbox{\begin{tabular}{l}\@pubnum\end{tabular}}}
      \titleskip
   \fi\fi
   \begin{center}
     {\setstretch{1.0}\Large\bf \@title\par}
     \titleskip
     Th\`{e}se pr\'{e}sent\'{e}e par\\
     {\large \@author\par}
     \authorskip
     En vue de l'obtention du grade de\\
     \@degreeaward\par
     \vfill
     \includegraphics[width=2in]{\@unilogo} \\
     \@university\\
     \@address\par
     \authorskip
     \iftechreport
        {\normalsize Technical Report \@pubnum\par}
     \else
     \@copyyear\par
        \ifthesisdraft
           {\normalsize (\@draftfoot)\par}
        \else
           \ifthesisdefended
              {\normalsize (Defended \@date)\par}
           \else
              {\normalsize (Submitted \@date)\par}
           \fi
        \fi
     \fi
   \end{center}
   \vspace*{.5in}
 \end{titlepage}
 \pagenumbering{roman}
 \setcounter{page}{2}
 \iftechreport
 \else
   \makecopyright
 \fi}
}               %% End of \maketitle

% The dedication page is not part of the Caltech thesis guidelines, but
% is an optional addition to the thesis. It is typically placed before
% the acknowledgments.
\newenvironment{dedication}
  {\parindent=0pt
   \vspace*{.1\textheight}
   \begin{center}}
  {\end{center}\newpage}

% The abstract and acknowledgments are treated as unnumbered chapters
% with Table of Contents entries.
\renewenvironment{abstract}
  {\extrachapter{Abstract}}
  {\clearpage}
\newenvironment{acknowledgments}
  {\extrachapter{Acknowledgments}}
  {\clearpage}
\newenvironment{abstractf}
  {\extrachapter{R\'esum\'e}}
  {\clearpage}
% although `acknowledgments' is preferred
\newenvironment{acknowledgements}
  {\extrachapter{Acknowledgements}}
  {\clearpage}

% The {thebibliography} environment should put an entry into the Table
% of Contents, just as the {abstract} and {acknowledgments} environments
% do.
\let\thesis@thebibliography\thebibliography

\def\thebibliography#1{\thesis@thebibliography{#1}%
  \addcontentsline{toc}{chapter}{Bibliography}}

% If we're using the lucida fonts, let's load them
\iflucidafont
  \RequirePackage{lucidbry}
\fi
\endinput
