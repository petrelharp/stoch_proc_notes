%%
%% This is file `fillsntsx.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% fillsclsx.dtx  (with options: `fillsntsx,class,fillsclsx')
%% 
%% This is a generated file.
%% 
%% Copyright (C) 2017 by Fill Staley
%% 
%% This file may be distributed and/or modified under the conditions
%%  of the LaTeX Project Public License,
%%  either version 1.3 of this license or (at your option) any later version.
%% The latest version of this license is in:
%% 
%% http://www.latex-project.org/lppl.txt
%% 
%%  and version 1.3 or later is part of all distributions
%%  of LaTeX version 2015/10/01 or later.
%% 
%% This work has the LPPL maintenance status `maintained'.
%% 
%% The Current Maintainer of this work is Fill Staley.
%% 
%% This work consists of the files fillsclsx.dtx
%%  and the derived files fillsntsx.cls, fillsttx.cls and fillsmathx.sty.
%% 
\NeedsTeXFormat{LaTeX2e}[2011/06/27]
\ProvidesClass{fillsntsx}
    [2017/01/07 v1.0 A suite of document classes and a package
        to be used for teaching and studying math.]
\global\expandafter\let\csname ver@fillsmathx.sty\expandafter\endcsname
    \csname ver@\@currname.\@currext\endcsname
\newdimen\f@tempdima
\newlength\fnts@ila
\newlength\fnts@ilb
\let\@xp\expandafter
\let\@nx\noexpand
\def\@addpunct#1{\relax\ifhmode
                 \ifnum\spacefactor>\@m \else #1 \fi
                 \fi}
\long\def\@nilgobble#1\@nil{}
\def\@True{00}
\def\@False{01}
\newcommand\newswitch[2][False]{%
    \@xp\@ifdefinable\csname ?@#2\endcsname{%
        \global\@xp\let\csname ?@#2\@xp\endcsname%
            \csname @#1\endcsname}%
    }
\newcommand{\setFalse}[1]{%
    \@xp\let\csname ?@#1\endcsname\@False}
\newcommand{\setTrue}[1]{%
    \@xp\let\csname ?@#1\endcsname\@True}
\newswitch{}
\DeclareRobustCommand\except[1]{%
    \if
        \csname ?@#1\endcsname \@xp\@gobble
    \else
        \@xp\@firstofone
    \fi}
\DeclareRobustCommand\for[1]{%
    \if
        \csname ?@#1\endcsname \@xp\@firstofone
    \else
        \@xp\@gobble
    \fi}
\DeclareRobustCommand\forany[1]{%
    \csname for@any01\endcsname#1,?,\@nil}
\@namedef{for@any@\@False}#1,{%
    \csname for@any@%
        \csname ?@\zap@space#1 \@empty\endcsname
    \endcsname}
\@namedef{?@?}{x}
\@namedef{for@any@\@True}#1\@nil#2{#2}
\def\for@any@x{\@car\@gobble}
\newcommand\uppercasenonmath[1]{\toks@\@emptytoks
    \@xp\@skipmath\@xp\@empty#1$$%
    \edef#1{{\@nx\protect\@nx\@upprep\the\toks@}}%
}
\newcommand\@upprep{%
    \spaceskip1.3\fontdimen2\font plus .3\fontdimen3\font
    \upchars@}
\newcommand{\upchars@}{%
    \def\ss{SS}\def\i{I}\def\j{J}\def\ae{\AE}\def\oe{\OE}%
    \def\o{\O}\def\aa{\AA}\def\l{\L}\def\Mc{M{\scshape c}}}
\providecommand\Mc{Mc}
\newcommand\@skipmath{}
\long\def\@skipmath#1$#2${%
    \@xskipmath#1\(\)%
    \@ifnotempty{#2}{\toks@\@xp{\toks@$#2$}\@skipmath\@empty}}%
\newcommand\@xskipmath{}
\long\def\@xskipmath#1\(#2\){%
    \uppercase{\toks@\@xp\@xp\@xp{\@xp\the\@xp\toks@#1}}%
    \@ifnotempty{#2}{\toks@\@xp{\the\toks@\(#2\)}\@xskipmath\@empty}}%
\def\altucnm#1{%
    \MakeTextUppercase{\toks@{#1}}%
    \edef#1{\the\toks@}%
}
\AtBeginDocument{%
    \@ifundefined{MakeTextUppercase}{}{\let\uperrcasenonmath\altucnm}}
\newif\iffcls@prftombstone\fcls@prftombstonefalse
\newif\iffcls@noflush\fcls@noflushfalse
\newif\iffcls@end
\newif\if@draft\@draftfalse
\newif\iffnts@chapters
\newif\iffnts@monograph\fnts@monographfalse
\newif\iffnts@classnotes\fnts@classnotesfalse
\newif\iffnts@book
\newif\ifftt@officehours
\newif\ifftt@appointment
\newif\ifftt@nograde\ftt@nogradefalse
\newif\ifftt@takehome\ftt@takehomefalse
\DeclareOption{empty}{\fcls@prftombstonetrue}
\DeclareOption{solid}{\fcls@prftombstonefalse}
\DeclareOption{noflush}{\fcls@noflushtrue}
\DeclareOption{flush}{\fcls@noflushfalse}
\DeclareOption{nochapters}{\fnts@chaptersfalse}
\DeclareOption{chapters}{\fnts@chapterstrue}
\DeclareOption{monograph}{
    \fnts@monographtrue
    \fnts@booktrue
    \fnts@chapterstrue
    \@twosidetrue
    \@mparswitchtrue}
\DeclareOption{classnotes}{
    \fnts@classnotestrue
    \fnts@booktrue
    \@twosidetrue
    \@mparswitchtrue}
\newif\iffnts@notitlesec\fnts@notitlesecfalse
\DeclareOption{notitlesec}{\fnts@notitlesectrue}
\DeclareOption{titlesec}{\fnts@notitlesecfalse}
\DeclareOption{a4paper}{
    \paperheight 297mm
    \paperwidth 210mm
    \textheight 54.5pc }
\DeclareOption{letterpaper}{
    \paperheight 11in
    \paperwidth 8.5in }
\DeclareOption{portrait}{}
\DeclareOption{landscape}{
    \f@tempdima\paperheight
    \paperheight\paperwidth
    \paperwidth\f@tempdima}
\DeclareOption{oneside}{
    \@twosidefalse
    \@mparswitchfalse}
\DeclareOption{twoside}{
    \@twosidetrue
    \@mparswitchtrue}
\DeclareOption{draft}{%
    \@drafttrue
    \overfullrule5\p@
    \ClassWarningNoLine{fillsntsx}{%
        When the draft option is used,
        the \protect\includegraphics\MessageBreak
        command will print blank placeholder boxes\MessageBreak
        for the graphics}}
\DeclareOption{final}{%
    \overfullrule\z@}
\newif\if@titlepage
\DeclareOption{titlepage}{
    \if@titlepagetrue}
\DeclareOption{notitlepage}{
    \if@titlepagefalse}
\newif\if@openright\@openrighttrue
\DeclareOption{openright}{
    \iffnts@chapters \@openrightture \fi}
\DeclareOption{openany}{
    \iffnts@chapters \@openrightfalse \fi}
\DeclareOption{onecolumn}{\@twocolumnfalse}
\DeclareOption{twocolumn}{\@twocolumntrue}
\newcommand\@mainsize{10}
\newcommand\@ptsize{0}
\newcommand\larger[1][1]{
    \count@\@currsizeindex \advance\count@#1\relax
    \ifnum\count@<\z@ \count@\z@ \else\ifnum\count@>12 \count@12\fi\fi
    \ifcase\count@
        \TINY\or\Tiny\or\tiny\or\SMALL\or\Small\or\small
    \or\normalsize
    \or\large\or\Large\or\LARGE\or\huge\or\Huge\else\HUGE
    \fi}
\newcommand\smaller[1][1]{\larger[-#1]}
\def\@adjustvertspacing{
    \bigskipamount.7\baselineskip plus.7\baselineskip
    \medskipamount\bigskipamount \divide\medskipamount\tw@
    \smallskipamount\medskipamount \divide\medskipamount\tw@
    \abovedisplayskip\medskipamount
    \belowdisplayskip \abovedisplayskip
    \abovedisplayshortskip\abovedisplayskip
    \advance\abovedisplayshortskip-1\abovedisplayskip
    \belowdisplayshortskip\abovedisplayshortskip
    \advance\belowdisplayshortskip 1\smallskipamount
    \jot\baselineskip \divide\jot 4 \relax}
\renewcommand\normalsize{\@xsetfontsize\normalsize 6%
    \@adjustvertspacing \let \@listi\@listI}
\DeclareRobustCommand\Tiny{\@xsetfontsize\Tiny 1}
\DeclareRobustCommand\tiny{\@xsetfontsize\tiny 2}
\DeclareRobustCommand\SMALL{\@xsetfontsize\SMALL 3}
\DeclareRobustCommand\Small{\@xsetfontsize\Small 4%
    \@adjustvertspacing
    \def\@listi{\topsep\smallskipamount \parsep\z@skip \itemsep\z@skip
        \leftmargin=\leftmargini
        \labelwidth=\leftmargini \advance\labelwidth-\labelsep}}
\DeclareRobustCommand\small{\@xsetfontsize\small 5%
    \@adjustvertspacing}
\def\footnotesize{\Small}
\def\scriptsize{\SMALL}
\DeclareRobustCommand\large{\@xsetfontsize\large 7
    \@adjustvertspacing}
\DeclareRobustCommand\Large{\@xsetfontsize\Large 8
    \@adjustvertspacing}
\DeclareRobustCommand\LARGE{\@xsetfontsize\LARGE 9}
\DeclareRobustCommand\huge{\@xsetfontsize\huge {10}}
\DeclareRobustCommand\Huge{\@xsetfontsize\Huge {11}}
\DeclareRobustCommand\HUGE{\@xsetfontsize\HUGE {12}}
\def\@xsetfontsize#1#2{%
    \chardef\@currsizeindex#2\relax
    \edef\@tempa{\@nx\@setfontsize\@nx#1%
        \@xp\ifcase\@xp\@currsizeindex\@typesizes
        \else{99}{99}\fi}%
    \@tempa}
\chardef\@currsizeindex=6
\widowpenalty=10000
\clubpenalty=10000
\brokenpenalty=10000
\newdimen\linespacing
\lineskip=1pt \lineskiplimit=1pt
\normallineskip=1pt \normallineskiplimit=1pt
\let\baselinestretch=\@empty
\headheight=8pt \headsep=14pt
\iffnts@book \footskip=18pt \else \footskip=12pt \fi
\textheight=50.5pc \topskip=10pt
\textwidth=30pc
\columnsep=10pt \columnseprule=0pt
\def\@pnumwidth{1.6em}
\def\@tocrmarg{2.6em}
\newskip\normaltopskip
\normaltopskip=10pt\relax
\marginparwidth=90pt
\marginparsep=11pt
\marginparpush=5pt
\AtBeginDocument{
    \settoheight{\footnotesep}{\footnotesize M$^1$}
    \savegeometry{default}}
\skip\footins=7pt plus 11pt
\skip\@mpfootins=\skip\footins
\fboxsep=3pt \fboxrule=.4pt

\arrayrulewidth=.4pt    \doublerulesep=2pt
\labelsep=5pt           \arraycolsep=\labelsep
\tabcolsep=\labelsep    \tabbingsep=\labelsep

\floatsep=15pt plus 12pt    \dblfloatsep=15pt plus 12pt
\textfloatsep=\floatsep     \dbltextfloatsep=15pt plus 12 pt
\intextsep=\floatsep

\@fptop=0pt plus 1fil   \@dblfptop=0pt plus 1fil
\@fpbot=0pt plus 1fil   \@dblfpbot=0pt plus 1fil
\@fpsep=8pt plus 2fil   \@dblfpsep=8pt plus 2fil\relax
\parskip=0pt \relax

\newdimen\normalparindent
\iffnts@book \normalparindent=18pt \else \normalparindent=12pt \fi
\parindent=\normalparindent

\partopsep=0pt \relax \parsep=0pt \relax \itemsep=0pt \relax

\@lowpenalty=51     \@medpenalty=151        \@highpenalty=301
\@beginparpenalty=-\@lowpenalty
\@endparpenalty=-\@lowpenalty
\@itempenalty=-\@lowpenalty
\let\cleardouble@page\cleardoublepage
\AtBeginDocument{
    \def\cleardoublepage{\clearpage{\pagestyle{empty}\cleardouble@page}}}
\DeclareOption{10pt}{\def\@mainsize{10}\def\@ptsize{0}%
    \def\@typesizes{%
        \or{5}{6}\or{6}{7}\or{7}{8}\or{8}{10}\or{9}{11}%
        \or{10}{12}% normalsize
        \or{\@xipt}{13}\or{\@xiipt}{14}\or{\@xivpt}{17}%
        \or{\@xviipt}{20}\or{\@xxpt}{24}\or{\@xxvpt}{30}}%
    \normalsize \linespacing=\baselineskip}
\DeclareOption{11pt}{\def\@mainsize{11}\def\@ptsize{1}%
    \def\@typesizes{%
        \or{6}{7}\or{7}{8}\or{8}{10}\or{9}{11}\or{10}{12}%
        \or{\@xipt}{13}% normalsize
        \or{\@xiipt}{14}\or{\@xivpt}{17}\or{\@xviipt}{20}%
        \or{\@xxpt}{24}\or{\@xxvpt}{30}\or{\@xxvpt}{30}}%
    \normalsize \linespacing=\baselineskip}
\DeclareOption{12pt}{\def\@mainsize{12}\def\@ptsize{2}%
    \def\@typesizes{%
        \or{7}{8}\or{8}{10}\or{9}{11}\or{10}{12}\or{\@xipt}{13}%
        \or{\@xiipt}{14}% normalsize
        \or{\@xivpt}{17}\or{\@xviipt}{20}\or{\@xxpt}{24}%
        \or{\@xxvpt}{30}\or{\@xxvpt}{30}\or{\@xxvpt}{30}}%
    \normalsize \linespacing=\baselineskip}
\DeclareOption{8pt}{\def\@mainsize{8}\def\@ptsize{8}%
    \def\@typesizes{%
        \or{5}{6}\or{5}{6}\or{5}{6}\or{6}{7}\or{7}{8}%
        \or{8}{10}% normalsize
        \or{9}{11}\or{10}{12}\or{\@xipt}{13}%
        \or{\@xiipt}{14}\or{\@xivpt}{17}\or{\@xviipt}{20}}%
    \normalsize \linespacing=\baselineskip}
\DeclareOption{9pt}{\def\@mainsize{9}\def\@ptsize{9}%
    \def\@typesizes{%
        \or{5}{6}\or{5}{6}\or{6}{7}\or{7}{8}\or{8}{10}%
        \or{9}{11}% normalsize
        \or{10}{12}\or{\@xipt}{13}\or{\@xiipt}{14}%
        \or{\@xivpt}{17}\or{\@xviipt}{20}\or{\@xxpt}{24}}%
    \normalsize \linespacing=\baselineskip}
\newif\iftagsleft@
\DeclareOption{leqno}{
    \tagsleft@true \PassOptionsToPackage{leqno}{amsmath}}
\DeclareOption{reqno}{
    \tagsleft@false \PassOptionsToPackage{reqno}{amsmath}}
\newif\ifctagsplit@
\DeclareOption{centertags}{
    \ctagsplit@true \PassOptionsToPackage{centertags}{amsmath}}
\DeclareOption{tbtags}{
    \ctagsplit@false \PassOptionsToPackage{tbtags}{amsmath}}
\DeclareOption{fleqn}{}
\DeclareOption{makeindx}{}
\ExecuteOptions{%
    letterpaper,portrait,onecolumn,final,%
    twoside,10pt,%
    leqno,centertags,%
    solid,flush}
\ProcessOptions\relax
\def\partname{Part}
\iffnts@chapters \def\chaptername{Chapter} \fi
\def\appendixname{Appendix}
\def\sectionname{Section}
\let\subsectionname\@empty
\let\subsubsectionname\@empty
\let\paragraphname\@empty
\let\subparagraphname\@empty
\newcommand\newname[2]{\@xp\def\csname #1name\endcsname{#2}}
\RequirePackage{lmodern}
\RequirePackage{etoolbox, ifthen, calc}
\RequirePackage{geometry, graphicx}
\RequirePackage[dvipsnames]{xcolor}
\RequirePackage[T1]{fontenc}
\RequirePackage{perpage}
\RequirePackage[strict]{changepage}
\RequirePackage{verbatim}
\RequirePackage{marginnote}
\RequirePackage{subfiles}
\RequirePackage{aliascnt}
\RequirePackage{amsmath, amsthm, amssymb}
\RequirePackage{mathtools,thmtools, mathrsfs}
\RequirePackage{framed}
    \providecommand{\bleftbar}{\begin{leftbar}}
    \providecommand{\eleftbar}{\end{leftbar}}
    \providecommand{\bframed}{\begin{framed}}
    \providecommand{\eframed}{\end{framed}}
\RequirePackage{tikz}
    \usetikzlibrary{cd, arrows}
    \usetikzlibrary{decorations.markings}
    \usetikzlibrary{decorations.pathreplacing}
    \tikzset{
        % style to apply some styles to each segment of a path
        on each segment/.style={
           decorate,
           decoration={
               show path construction,
               moveto code={},
               lineto code={
                   \path [#1]
                   (\tikzinputsegmentfirst) -- (\tikzinputsegmentlast);
                },
                curveto code={
                    \path [#1] (\tikzinputsegmentfirst)
                    .. controls
                    (\tikzinputsegmentsupporta) and (\tikzinputsegmentsupportb)
                    ..
                    (\tikzinputsegmentlast);
                },
                closepath code={
                    \path [#1]
                    (\tikzinputsegmentfirst) -- (\tikzinputsegmentlast);
                    },
                },
            },
        % style to add an arrow in the middle of a path
        mid arrow/.style={
            postaction={
                decorate,decoration={
                    markings,
                    mark=at position .5 with {\arrow[#1]{stealth}}
                    }
                }
            },
        }
\iffnts@notitlesec  \RequirePackage[nopatches]{titleps} \else
\RequirePackage[explicit,calcwidth,loadonly,pagestyles]{titlesec}
\let\ttl@atbegin\relax
\fi
\RequirePackage{titletoc}
\RequirePackage[%
    obeyFinal, colorinlistoftodos, textwidth=1.25in, shadow]{todonotes}
\PassOptionsToPackage{shortlabels, inline}{enumitem}
\PassOptionsToPackage{hidelinks, final}{hyperref}
\AtBeginDocument
   {\RequirePackage{array}
    \RequirePackage{enumitem}
    \@ifpackageloaded{varioref}
       {\@ifpackageloaded{hyperref}
           {\RequirePackage{cleveref}}
           {\RequirePackage{hyperref}
            \RequirePackage{cleveref}}}
       {\RequirePackage{varioref}
        \@ifpackageloaded{hyperref}
           {\ClassWarningNoLine{fillsntsx}{
                The `varioref' package must be loaded\MessageBreak
                before the `hyperref' package}
            \RequirePackage{cleveref}}
           {\RequirePackage{hyperref}
            \RequirePackage{cleveref}}}}
\MakePerPage[2]{footnote}
\renewcommand{\thefootnote}{\fnsymbol{footnote}}
\def\footnoterule{\kern-.4\p@
        \hrule\@width 5pc\kern11\p@\kern-\footnotesep}
\def\@makefnmark{
        \leavevmode
        \raise.9ex\hbox{\fontsize\sf@size\z@\normalfont\@thefnmark}}
\def\@makefntext{\indent\@makefnmark}
\long\def\@footnotetext#1{
        \insert\footins{
                \normalfont\footnotesize
                \interlinepenalty\interfootnotelinepenalty
                \splittopskip\footnotesep \splitmaxdepth \dp\strutbox
                \floatingpenalty\@MM \hsize\columnwidth
                \@parboxrestore \parindent\normalparindent \sloppy
                \protected@edef\@currentlabel{
                        \csname p@footnote\endcsname\@thefnmark}
                \@makefntext{
                        \rule\z@\footnotesep\ignorespaces#1\unskip\strut\par}}}
\hfuzz=1pt \vfuzz=\hfuzz
\def\sloppy{\tolerance9999 \emergencystretch 3em\relax}
\renewpagestyle{empty}{}
\renewpagestyle{plain}{}
\newpagestyle{firstpage}{}
\newpagestyle{headings}{
    \sethead[\thepage \hspace*{0.5in} {}][][]
    {}{}{{} \hspace*{0.5in}\thepage}}
\newpagestyle{notes}{}
\newpagestyle{monograph}{}
\newpagestyle{lecture}{}
\newpagestyle{assignment}{\sethead{\fnts@coursename}{}{}}
\newcounter{part}
\iffnts@chapters
    \newcounter{chapter}
    \newcounter{section}[chapter]
\else
    \newcounter{section}[part]
\fi
\newcounter{subsection}[section]
\newcounter{subsubsection}[subsection]
\newcounter{paragraph}[subsubsection]
\newcounter{subparagraph}[paragraph]
\renewcommand\thepart{\Roman{part}}
\iffnts@chapters \renewcommand\thechapter{\arabic{chapter}} \fi
\renewcommand\thesection{\arabic{section}}
\renewcommand{\thesubsection}{\thesection.\arabic{subsection}}
\renewcommand\theparagraph{\Roman{paragraph}}
\iffnts@chapters
    \setcounter{secnumdepth}{2}
\else
    \setcounter{secnumdepth}{1}
\fi
\iffnts@notitlesec\else
\titleclass{\part}[-1]{top}
\iffnts@chapters
    \titleclass{\chapter}{top}[\part]
    \titleclass{\section}{straight}[\chapter]
\else
    \titleclass{\section}{straight}[\part]
\fi
\titleclass{\subsection}{straight}[\section]
\titleclass{\subsubsection}{straight}[\subsection]
\titleclass{\paragraph}{straight}[\subsubsection]
\titleclass{\subparagraph}{straight}[\paragraph]
\titleformat{\part}[display]
    {\thispagestyle{empty}}
    {\Large\bfseries \partname\ \thepart}
    {1.5pc}
    {\hspace*{1em}\parbox[c][3\baselineskip][l]{0.75\textwidth}{\huge #1}}
    [\f@partsummary
     \f@parthook]
\newcommand\partsummaryname{Summary}
\newbox\partsummarybox
\newenvironment{summary}{
    \global\setbox\partsummarybox=\vtop \bgroup
    \normalfont\Small
    \list{}{\labelwidth\z@
        \leftmargin6pc \rightmargin\leftmargin
        \listparindent\normalparindent \itemindent\z@
        \parsep\z@ \@plus\p@
        \let\fullwidthdispaly\relax}
    \item[\hskip\labelsep\scshape\partsummaryname.]}
{\endlist\egroup}
\def\f@partsummary{
    \ifvoid\partsummarybox
    \else
        \skip@20\p@ \advance\skip@-\lastskip \advance\skip@-\baselineskip
        \vskip\skip@
        \box\partsummarybox
        \prevdepth\z@ % because \abstract box is a vtop
        \vspace{1.5\baselineskip}
    \fi}
\def\f@parthook{}
\iffnts@book
    \def\f@parthook{
        \f@partcontents
        \cleardoublepage}
\fi
\def\f@partcontents{
    \startcontents[part]
    \printcontents[part]{p}{0}{%
        \unskip
        \setcounter{tocdepth}{2}
        \contentsmargin{0pc}
        \settowidth{\fnts@ila}{\bfseries\contentsname}
        \addtolength{\fnts@ila}{2em}
        \centering
        \begingroup
            {\bfseries\contentsname}\par
            \rule[0.9ex]{\fnts@ila}{1pt}\par
        \endgroup}}
\titlespacing*{\part}
    {0pc} % left
    {0pc} % before
    {2.5pc} % after
\iffnts@chapters
\newlength\fnts@chapterrule
\titleformat{\chapter}[display]%
    {\bfseries\LARGE\filleft}%
    {\chaptername\ \thechapter}% format for name and number
    {1em}% space between label and title
    {\settowidth\fnts@ila{\chaptername\ \thechapter} % the length of the label
     \ifthenelse%
        {\lengthtest{\titlewidth > \fnts@ila}}% the title is longer, then
        {\setlength\fnts@chapterrule{\titlewidth}}% use the title,
        {\setlength\fnts@chapterrule{\fnts@ila}}% else use the label
     \hfill\rule[1\baselineskip]{\fnts@chapterrule}{0.8pt} \newline #1}%
\titleformat{name=\chapter,numberless}[block]
    {\bfseries\LARGE}
    {}
    {7.5em}
    {\filcenter\MakeUppercase #1}
\titlespacing*{\chapter}
    {0pt} % left
    {5em} % before
    {10em plus .5\baselineskip minus .5\baselineskip} % after
    [4em] % right
\fi
\renewcommand\sectionname{\S}
\newlength\fnts@ssdepth
\settodepth\fnts@ssdepth{\sectionname}
\titleformat\section
    {\bfseries\Large}
    {\sectionname\ \thesection
        \hspace{3pt} \rule[-2\fnts@ssdepth]{1pt}{\baselineskip}}
    {1pc}
    {\parbox[c]{0.8\linewidth}{#1}}
\titlespacing\section{0pt}
    {1.7\baselineskip plus 0.3\baselineskip minus 0.2\baselineskip}
    {1\baselineskip minus 0.2\baselineskip}
\titleformat\subsection[display]
    {\bfseries}
    {\underline{\raisebox{3pt}{\footnotesize \thesubsection \hspace{1pc}}}}
    {0pt}
    {#1}
\titlespacing\subsection{0pc}
    {1\baselineskip plus 0.2\baselineskip minus 0.1\baselineskip}
    {0.4\baselineskip plus 0.1\baselineskip minus 0.1\baselineskip}
\titleformat{\subsubsection}[hang]
    {\normalfont\itshape\large}
    {}
    {0pt}
    {#1}
\titlespacing*{\subsubsection}{2.5pc}
    {1\linespacing plus 0.5\linespacing}
    {0.3\linespacing}
\titleformat\paragraph[runin]
    {\normalfont\bfseries}
    {\theparagraph}
    {0ex}
    {#1.}
\titlespacing{\paragraph}{1\parindent}
    {0pt}
    {0.5ex}
\fi
\iffnts@book
    \def\frontmatter{\cleardoublepage
        \restoregeometry
        \pagenumbering{roman}\setcounter{secnumdepth}{-1}
        \setcounter{tocdepth}{1}}
    \def\mainmatter{\cleardoublepage
        \restoregeometry
        \pagenumbering{arabic}\setcounter{secnumdepth}{2}}
    \def\backmatter{
        \restoregeometry
        \if@openright\cleardoublepage\else\clearpage\fi
        \let\chaptername\relax}
\fi
\def\labelenumi{(\theenumi)}        \def\theenumi{\@arabic\c@enumi}
\def\labelenumii{(\theenumii)}      \def\theenumii{\@alph\c@enumii}
\def\labelenumiii{(\theenumiii)}    \def\thenumiii{\@roman\c@enumiii}
\def\labelenumiv{\theenumiv.}       \def\theenumiv{\@Alph\c@enumiv}

\def\p@enumii{\theenumi}
\def\p@enumiii{\theenumi(\theenumii)}
\def\p@enumiv{\p@enumiii\theenumiii}

\def\labelitemi{$\m@th\bullet$}
\def\labelitemii{\bfseries --}
\def\labelitemiii{$\m@th\ast$}
\def\labelitemiv{$\m@th\cdot$}
\newenvironment{verse}{\let\\\@centercr
    \list{}{%
        \itemsep        \z@
        \itemindent     -1.5em
        \listparindent  \itemindent
        \rightmargin    \leftmargin
        \advance\leftmargin 1.5em}
    \item[]}
    {\endlist}
\let\endverse\endlist % for efficiency

\newenvironment{quotation}
    {\list{}{%
        \leftmargin     3pc
        \listparindent  \normalparindent
        \itemindent     \z@
        \rightmargin    \leftmargin
        \parsep         \z@ \@plus \z@}
    \item[]}
    {\endlist}
\let\endquotation\endlist % for efficiency

\newenvironment{quote}
    {\list{}{\rightmargin\leftmargin}\item[]}
    {\endlist}
\let\endquote\endlist % for efficiency
\def\trivlist{\parsep\parskip\@nmbrlistfalse
    \@trivlist  \labelwidth     \z@
                \leftmargin     \z@
                \itemindent     \z@
                \let\@itemlabel \@empty
    \def\makelabel##1{\upshape##1}}

\renewenvironment{enumerate}
    {\ifnum \@enumdepth >3 \@toodeep\else
        \advance\@enumdepth \@ne
        \edef\@enumctr{enum\romannumeral\the\@enumdepth}
        \list{\csname label\@enumctr\endcsname}{
            \usecounter{\@enumctr}
            \def\makelabel##1{\hss\llap{\upshape##1}}}
    \fi}
    {\endlist}
\let\endenumerate\endlist % for efficiency

\renewenvironment{itemize}
    {\ifnum\@itemdepth>3 \@toodeep
        \else \advance\@itemdepth\@ne
        \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}
        \list{\csname \@itemitem\endcsname}{
        \def\makelabel##1{\hss\llap{\upshape##1}}}
    \fi}
    {\endlist}
\let\enditemize\endlist % for efficiency

\newcommand\descriptionlabel[1]{\hspace\labelsep \upshape\bfseries #1:}
\newenvironment{description}
    {\list{}{
        \advance\leftmargini    6\p@
        \itemindent             -12\p@
        \labelwidth             \z@
        \let\makelabel\descriptionlabel}}
    {\endlist}
\let\enddescription\endlist % for efficiency
\let\upn=\textup
\AtBeginDocument{%
    \labelsep=5pt\relax
    \setcounter{enumi}{13}
    \setcounter{enumii}{13}
    \setcounter{enumiii}{13}
    \setcounter{enumiv}{13}
    \settowidth\leftmargini{\labelenumi\hskip\labelsep}
    \advance\leftmargini by \normalparindent
    \settowidth\leftmarginii{\labelenumii\hskip\labelsep}
    \settowidth\leftmarginiii{\labelenumiii\hskip\labelsep}
    \settowidth\leftmarginiv{\labelenumiv\hskip\labelsep}
    \setcounter{enumi}{0}
    \setcounter{enumii}{0}
    \setcounter{enumiii}{0}
    \setcounter{enumiv}{0}
    \leftmarginv=10pt       \leftmarginvi=\leftmarginv
    \leftmargin=\leftmargini
    \labelwidth=\leftmargini    \advance\labelwidth-\labelsep
    \@listi}
\newskip\listisep
\listisep\smallskipamount
\def\@listI{\leftmargin\leftmargini     \parskip\z@skip
            \topsep\listisep            \itemsep\z@skip
            \listparindent\normalparindent}
\let\@listi\@listI
\def\@listii{\leftmargin\leftmarginii
    \labelwidth\leftmarginii    \advance\labelwidth-\labelsep
    \topsep\z@skip  \parsep\z@skip  \partopsep\z@skip   \itemsep\z@skip}
\def\@listiii{\leftmargin\leftmarginiii
    \labelwidth\leftmarginiii   \advance\labelwidth-\labelsep}
\def\@listiv{\leftmargin\leftmarginiv
    \labelwidth\leftmarginiv    \advance\labelwidth-\labelsep}
\def\@listv{\leftmargin\leftmarginv
    \labelwidth\leftmarginv     \advance\labelwidth-\labelsep}
\def\@listvi{\leftmargin\leftmarginvi
    \labelwidth\leftmarginvi    \advance\labelwidth-\labelsep}
\setcounter{topnumber}{4}
\setcounter{bottomnumber}{4}
\setcounter{totalnumber}{4}
\setcounter{dbltopnumber}{4}
\renewcommand{\topfraction}{.97}
\renewcommand{\bottomfraction}{.97}
\renewcommand{\textfraction}{.03}
\renewcommand{\floatpagefraction}{.9}
\renewcommand{\dbltopfraction}{.97}
\renewcommand{\dblfloatpagefraction}{.9}
\setlength{\floatsep}{12pt plus 6pt minus 4pt}
\setlength{\textfloatsep}{15pt plus 8pt minus 5pt}
\setlength{\intextsep}{12pt plus 6pt minus 4pt}
\setlength{\dblfloatsep}{12pt plus 6pt minus 4pt}
\setlength{\dbltextfloatsep}{15pt plus 8pt minus 5pt}
\setlength{\@fptop}{0pt}% removed ``plus 1fil''
\setlength{\@fpsep}{8pt}% removed ``plus 2fil''
\setlength{\@fpbot}{0pt plus 1fil}
\setlength{\@dblfptop}{0pt}% removed ``plus 1fil''
\setlength{\@dblfpsep}{8pt}% removed ``plus 2fil''
\setlength{\@dblfpbot}{0pt plus 1fil}
\newcommand{\fps@figure}{tbp}
\newcommand{\fps@table}{tbp}
\iffnts@chapters   \newcounter{figure}[chapter] \else \newcounter{figure}  \fi
\def\@captionheadfont{\scshape}
\def\@captionfont{\normalfont}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename\ \thefigure}
\def\figurename{Figure}
\newenvironment{figure}{
    \@float{figure}}{
    \end@float}
\newenvironment{figure*}{
    \@dblfloat{figure}}{
    \end@dblfloat}
\iffnts@chapters    \newcounter{table}[chapter] \else \newcounter{table}  \fi
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename\ \thetable}
\def\tablename{Table}
\newenvironment{table}{
    \@float{table}}{
    \end@float}
\newenvironment{table*}{
    \@dblfloat{table}}{
    \end@dblfloat}
\long\def\@makecaption#1#2{
    \setbox\@tempboxa\vbox{\color@setgroup
        \advance\hsize-2\captionindent\noindent
        \@captionfont\@captionheadfont#1\@xp\@ifnotempty\@xp
        {\@cdr#2\@nil}{.\@captionfont\upshape\enspace#2}
        \unskip\kern-2\captionindent\par
        \global\setbox\@ne\lastbox\color@endgroup}
    \ifhbox\@ne % the normal case
        \setbox\@ne\hbox{\unhbox\@ne\unskip\unskip\unpenalty\unkern}
    \fi
    \ifdim\wd\@tempboxa=\z@ % this means caption will fit on one line
        \setbox\@ne\hbox to\columnwidth{\hss\kern-2\captionindent\box\@ne\hss}
    \else % tempboxa contained more than one line
        \setbox\@ne\vbox{\unvbox\@tempboxa\parskip\z@skip
            \noindent\unhbox\@ne\advance\hsize-2\captionindent\par}
    \fi
    \ifnum\@tempcnta<64 % if the float IS a figure...
        \addvspace\abovecaptionskip
        \hbox to\hsize{\kern\captionindent\box\@ne\hss}
    \else % if the float IS NOT a figure...
        \hbox to\hsize{\kern\captionindent\box\@ne\hss}
        \nobreak
        \vskip\belowcaptionskip
    \fi
    \relax
}
\newskip\abovecaptionskip \abovecaptionskip=12pt \relax
\newskip\belowcaptionskip \belowcaptionskip=12pt \relax
\newdimen\captionindent \captionindent=3pc
\renewcommand*\title{\@dblarg\fnts@title}
\def\fnts@title[#1]#2{\gdef\@shorttitle{#1}  \gdef\@title{#2}}
\let\@shorttitle\@empty
\newcommand*\subtitle{\@dblarg\fnts@subtitle}
\def\fnts@subtitle[#1]#2{\gdef\@shortsubtitle{#1}  \gdef\@subtitle{#2}}
\let\@subtitle\@empty
\renewcommand\author{\@dblarg\fnts@author}
\def\fnts@author[#1]#2{
    \ifx\@authors\@empty
        \gdef\shortauthors{#1}  \gdef\@authors{#2}
        \g@addto@macro\@addresses{\author{#1}}
    \else
        \g@addto@macro\@shortauthors{\and#1}  \g@addto@macro\@authors{\and#2}
        \g@addto@macro\@addresses{\author{#1}}
    \fi}
\let\@shortauthors\@empty  \let\@authors\@empty  \let\@addresses\@empty
\newcommand\address[2][]{\g@addto@macro\@addresses{\address{#1}{#2}}}
\newcommand\curraddr[2][]{\g@addto@macro\@addresses{\curraddr{#1}{#2}}}
\newcommand\email[2][]{\g@addto@macro\@addresses{\email{#1}{#2}}}
\newcommand\urladdr[2][]{\g@addto@macro\@addresses{\urladdr{#1}{#2}}}
\long\def\thanks@warning#1{
    \ClassError{fillsntsx}{
        \protect\thanks\space should be given separately,
            not inside author name.}\@ehb}
\renewcommand\thanks[1]{
    \@ifnotempty{#1}{\g@addto@macro\@thankses{\thanks{#1}}}}
\let\@thankses\@empty
\long\def\dedicatory#1{\gdef\@dedicatory{#1}}
\def\keywords#1{\def\@keywords{#1}}
\@namedef{subjclassname@1991}{\textup{1991} Mathematics Subject Classification}
\@namedef{subjclassname@2000}{\textup{2000} Mathematics Subject Classification}
\@namedef{subjclassname@2010}{\textup{2010} Mathematics Subject Classification}
\@xp\let\@xp\subjclassname\csname subjclassname@2010\endcsname
\providecommand\subjclass{}
\renewcommand*\subjclass[2][2010]{
    \def\@subjclass{#2}
    \@ifundefined{subjclassname@#1}
        {\ClassWarning{fnts}{Unknown edition (#1) of Mathematics Subject
            Classification; using '2010'.}}
        {\@xp\let\@xp\subjclassname\csname subjclassname@#1\endcsname}}
\let\@dedicatory\@empty  \let\@keywords\@empty  \let\@subjclass\@empty
\newcommand\abstractname{Abstract}
\newbox\abstractbox
\newenvironment{abstract}{
    \ifx\maketitle\relax
    \ClassWarning{fillsntsx}{
        Abstract should precede \protect\maketitle\space}
    \fi
    \global\setbox\abstractbox=\vtop \bgroup
    \normalfont\Small
    \list{}{\labelwidth\z@
        \leftmargin6pc \rightmargin\leftmargin
        \listparindent\normalparindent \itemindent\z@
        \parsep\z@ \@plus\p@
        \let\fullwidthdispaly\relax}
    \item[\hskip\labelsep\scshape\abstractname.]}
{\endlist\egroup
    \ifx\@setabstract\relax \@setabstracta \fi}
\let\@date\@empty
\newcommand\today{
    \relax\number\day\space \ifcase\month\or
    January\or February\or March\or April\or May\or June\or July\or
    August\or September\or October\or November\or December\fi\space
    \number\year}
\newcommand{\xandlist}[4]{\@andlista{{#1}{#2}{#3}}#4\and\and}
\def\@andlista#1#2\and#3\and{\@andlistc{#2}\@ifnotempty{#3}{%
        \@andlistb#1{#3}}}
\def\@andlistb#1#2#3#4#5\and{%
    \@ifempty{#5}{%
        \@andlistc{#2#4}%
    }{%
        \@andlistc{#1#4}\@andlistb{#1}{#3}{#3}{#5}%
}}
\let\@andlistc\@iden
\newcommand{\nxandlist}[4]{%
    \def\@andlistc##1{\toks@\@xp{\the\toks@##1}}%
    \toks@{\toks@\@emptytoks \@andlista{{#1}{#2}{#3}}}%
    \the\@xp\toks@#4\and\and
    \edef#4{\the\toks@}%
    \let\@andlistc\@iden}
\def\@@and{and}
\newcommand{\andify}{%
    \nxandlist{\unskip, }{\unskip{} \@@and~}{\unskip, \@@and~}}
\def\and{\unskip{ }\@@and{ }\ignorespaces}
\newcommand\courseinfo{
    \@testopt{\fnts@courseinfo}{MATH}}
\def\fnts@courseinfo[#1]#2#3{
    \@testopt{\fnts@courseinfoi[#1]{#2}{#3}}{#3}}
\def\fnts@courseinfoi[#1]#2#3[#4]#5{
    \@testopt{\fnts@courseinfoii[#1]{#2}{#3}[#4]{#5}}{}}
\def\fnts@courseinfoii[#1]#2#3[#4]#5[#6]{
    \@testopt{\fnts@courseinfoiii[#1]{#2}{#3}[#4]{#5}[#6]}{}}
\long\def\fnts@courselist{}
\def\fnts@courseinfoiii[#1]#2#3[#4]#5[#6][#7]{
    \gdef\fnts@courseid{#1 #2}
    \gdef\fnts@coursename{#3}
    \gdef\fnts@courseterm{#5}
    \gdef\fnts@courseinstr{#6}
    \gdef\fnts@coursecrn{#7}
    \fnts@coursetermparse#5.
    \fnts@coursestore{#4}
    \g@addto@macro\fnts@courselist{
        \item[#1 #2] \fnts@clsep #3 \par\nointerlineskip #5}
    \ifx#6\@empty\else
        \g@addto@macro\fnts@courselist{ --- #6}
    \fi
    \ifx#7\@empty\else
        \g@addto@macro\fnts@courselist{\hspace{1em}
            \bgroup\footnotesize\textsc{CRN}: #7\egroup}
    \fi}
\def\fnts@coursetermparse#1 #2.{
    \gdef\fnts@coursequarter{#1}
    \gdef\fnts@courseyear{#2}}
\def\fnts@coursestore#1{
    \expandafter\let \csname courseid@#1\endcsname \fnts@courseid
    \expandafter\let \csname coursename@#1\endcsname \fnts@coursename
    \expandafter\let \csname courseterm@#1\endcsname \fnts@courseterm}
\def\course#1{
    \renewcommand{\fnts@courseid}{\csname courseid@#1\endcsname}
    \renewcommand{\fnts@coursename}{\csname coursename@#1\endcsname}
    \renewcommand{\fnts@courseterm}{\csname courseterm@#1\endcsname}
    \let\CourseName\fnts@coursename}
\let\CourseName\@empty
\def\amsart@maketitle{
    \thispagestyle{firstpage}
    \normalfont\normalsize
    \amsart@adminfootnotes
    \global\topskip42\p@\relax
    \amsart@settitle
    \ifx\@empty\@authors \else \amsart@setauthors  \fi
    \@setabstract
    \normalsize
    \if@titlepage
        \newpage
    \else
        \dimen@34\p@ \advance \dimen@-\baselineskip
        \vskip\dimen@\relax
    \fi
    \setcounter{page}{1}}
\def\amsartmaketitle{
    \par
    \@topnum\z@
    \uppercasenonmath\@shorttitle
    \ifx\@empty\@shortaurthors \let\@shortauthors\@shorttitle
    %    \else \andify\@shortauthors
    \fi
    \amsart@maketitle@hook
    \begingroup
        \amsart@maketitle
    \endgroup
    \c@footnote\z@
    \fnts@cleartopmattertags}
\def\amsart@adminfootnotes{
    \let\@makefnmark\relax  \let\@thefnmark\relax
    \ifx\@empty\@date   \else   \@footnotetext{\amsart@setdate}\fi}
\def\amsart@setdate{\datename\ \@date\@addpunct.}
\newcommand\datename{\textit{Date}:}
\def\amsart@settitle{
    \centering
    \baselineskip14\p@\relax
    {\bfseries\uppercasenonmath\@title \@title} \par
    {\bfseries\@subtitle}}
\def\amsart@setauthors{
    \begingroup
        \trivlist
            \centering
            \footnotesize \@topsep30\p@\relax
            \advance\@topsep by -\baselineskip
            \item\relax
            \def\\{\protect\linebreak}
            \MakeUppercase{\@authors} \par
        \endtrivlist
    \endgroup}
\def\amsart@maketitle@hook{\global\let\amsart@maketitle@hook\@empty}
\iffnts@book
\def\@maketitle{
    \cleardoublepage    \thispagestyle{empty}
    \begingroup
        \openup\bigskipamount
        \fnts@setauthors\par
        \vfil
        \fnts@settitle
        \vfil
        \flushleft
        \flushbottom
        \fnts@setcourselist
        \fnts@setdate
    \endgroup
    \newpage \thispagestyle{empty}
    \fnts@setaddresses
    \vfil
    \bigskip
    \@setabstract
    \vfil
    \bigskip\bigskip
    \fnts@setsubjclass
    \bigskip
    \fnts@setkeywords
    \vfil
    \cleardoublepage \setcounter{page}{1}}
\def\maketitle{
    \newgeometry{margin=1in, nomarginpar, noheadfoot}
    \@topnum\z@
    \@maketitle@hook
    \begingroup
    \@maketitle
    \endgroup
    \c@footnote\z@
    \fnts@cleartopmattertags
    \loadgeometry{default}}
\def\@maketitle@hook{\global\let\@maketitle@hook\@empty}
\else
\let\maketitle\amsartmaketitle
\fi
\def\fnts@cleartopmattertags{
        \def\do##1{\let##1\relax}
        \do\maketitle \do\@maketitle \do\title \do\@title
        \do\subtitle \do\@subtitle \do\author \do\@authors
        \do\address \do\curraddr \do\email \do\urladdr \do\@addresses
        \do\dedicatory \do\@dedicatory \do\thanks \do\@thankses
        \do\keywords \do\@keywords \do\subjclass \do\@subjclass}
\def\titlepage{
    \iffnts@book
        \cleardoublepage
    \else
        \clearpage
    \fi
    \thispagestyle{empty} \setcounter{page}{0}}
\def\endtitlepage{\newpage}
\def\fnts@settitle{
    \begin{flushright}
        \def\fnts@temp{%
            \Huge\bfseries \@title}
        \fnts@temp\par
        \settowidth\f@tempdima\fnts@temp
        \addtolength\f@tempdima{3pc}
        \rule\f@tempdima{0.4pt}\par\bigskip
    \end{flushright}
    \ifx\@subtitle\@empty \else
        \begin{flushright}
            \Large\bfseries \@subtitle\par\bigskip
        \end{flushright}
    \fi}
\def\fnts@setauthors{
    \begin{flushleft}
        \def\and{\par\medskip}
        \Large\mdseries \@authors\par\bigskip
    \end{flushleft}}
\def\fnts@setdate{
    \if@draft
        \begin{flushright}
        \renewcommand\datename{Printed on}
        \datename\ \@date\@addpunct.
        \end{flushright}
    \fi}
\def\fnts@setcourselist{
    \begin{flushleft}
        \def\fnts@clsep{\hspace{1ex}%
            \raisebox{0.255ex}\fnts@clsepsym\hspace{1ex}}
        \let\fnts@clsepsym\textbullet
        \settowidth\fnts@ila{Math 123}
        \list{}{\labelwidth=\fnts@ila \leftmargin=\labelwidth
                \labelsep=0pt \listparindent=2.5pc
                \parsep=1.3ex \itemsep=1.5\bigskipamount}
            \fnts@courselist
        \endlist
    \end{flushleft}}
\def\curraddrname{{\itshape Current Address}}
\def\emailaddrname{{\itshape E-mail address}}
\def\urladdrname{{\itshape URL}}
\def\fnts@setaddresses{
    \begin{flushleft}
        \nobreak
        \begingroup
            \def\author##1{\nobreak\addvspace\bigskipamount##1\par\medskip}
            \interlinepenalty\@M
            \def\address##1##2{
                \begingroup
                    \nobreak\indent
                    \ifx##1\@empty \else (\ignorespaces##1\unskip) \fi
                    {\scshape\ignorespaces##2}\par
                    \smallskip
                \endgroup}
            \def\curraddr##1##2{
                \begingroup
                    \ifx##2\@empty \else \nobreak\indent\curraddrname
                        \ifx##1\@empty \else
                            {, \ignorespaces##1\unskip} \fi
                        \/:\space \par
                        \parshape=1 1pc 0.5\linewidth ##2\par
                    \fi
                \endgroup}
            \def\email##1##2{
                \begingroup
                    \ifx##2\@empty \else \nobreak\indent\emailaddrname
                        \ifx##1\@empty \else
                            {, \ignorespaces##1\unskip} \fi
                        \/:\space \ttfamily##2\par
                    \fi
                \endgroup}
            \def\urladdr##1##2{
                \begingroup
                    \def~{\char'\~}
                    \ifx##2\@empty \else \nobreak\indent\urladdrname
                        \ifx##1\@empty \else
                            {, \ignorespaces##1\unskip} \fi
                        \/:\space \ttfamily##2\par
                    \fi
                \endgroup}
            \@addresses
        \endgroup
    \end{flushleft}}
\def\@setabstract{\@setabstracta \global\let\@setabstract\relax}
\def\@setabstracta{
    \ifvoid\abstractbox
    \else
        \skip@20\p@ \advance\skip@-\lastskip \advance\skip@-\baselineskip
        \vskip\skip@
        \box\abstractbox
        \prevdepth\z@ % because \abstract box is a vtop
    \fi}
\def\fnts@setsubjclass{\noindent
    \parshape=2 0pt 6.25in 18pt 6in
    {\itshape\subjclassname.}\enspace\@subjclass\@addpunct.\par}
\def\keywordsname{Key words and phrases}
\def\fnts@setkeywords{\noindent
    \parshape=2 0pt 6.25in 18pt 6in
    {\itshape\keywordsname.}\enspace\@keywords\@addpunct.\par}
\def\fnts@setdedicatory{}
\def\fnts@setthanks{
    \def\thanks##1{\par##1\@addpunct.}\thankses}
\iffnts@book
    \setcounter{tocdepth}{1}
\else
    \setcounter{tocdepth}{3}
\fi
\newswitch{toc}
\def\contentsname{Contents}
\def\tableofcontents{\@starttoc{toc}\contentsname}
\def\@starttoc#1#2{%
    \begingroup
        \setTrue{#1}%
        \begingroup
            \if@openright\cleardoublepage\else\clearpage\fi
            \thispagestyle{plain}\global\@topnum\z@
            \@afterindenttrue
        \endgroup
        \let\@secnumber\@empty
        \ifx\contentsname#2\else
            \addcontentsline{toc}{chapter}{#2}\fi
        \typeout{#2}\expandafter\chaptermark\expandafter{#2}%
        \@makeschapterhead{#2}\@afterheading
        \parskip\z@skip
        \makeatletter
        \@input{\jobname.#1}%
        \if@filesw
            \@xp\newwrite\csname tf@#1\endcsname
            \immediate\@xp\openout\csname tf@#1\endcsname \jobname.#1\relax
        \fi
        \global\@nobreakfalse
    \endgroup
    \newpage}
\def\@makeschapterhead#1{\global\topskip 1.5pc\relax
    \begingroup
    \Large\bfseries\centering
    #1\par \endgroup
    \skip@34\p@ \advance\skip@-\normalbaselineskip
    \vskip\skip@ }
\titlecontents{part}[1pc]%
    {\addvspace{10pt}\bfseries\titlerule[2pt]\addvspace{4pt}\large}%
    {}%
    {\contentspush{\partname\ \thecontentslabel}}%
    {\hfill\thecontentspage}%
    %[\addvspace{1\baselineskip plus 0.5\baselineskip minus 0.2\baselineskip}]
\titlecontents{chapter}[2pc]%
    {\addvspace{3pt}\contentsmargin{3.2em}\scshape}%
    {\chaptername\ \thecontentslabel: \hspace{1ex}}%
    {}%
    {\hfill\normalfont\thecontentspage}%
    %[\addvspace{0.3\baselineskip minus 0.05\baselineskip}]
\titlecontents{section}[4pc]%
    {\contentsmargin{3.2em}}%
    {\sectionname\ \thecontentslabel \hspace{0.8em}}%
    {}%
    {\hfill\thecontentspage}%
\titlecontents{subsection}[4pc]
    {\contentsmargin{3.2em}}
    {\thecontentslabel\ \hspace{1.3em}}
    {}
    {\hfill\thecontentspage}
\titlecontents*{subsubsection}[7.5pc]
    {\contentsmargin{6.5pc}\itshape}
    {\thecontentslabel}
    {}
    {\hspace{1ex} \thecontentspage}
    [\hspace{1em} $\diamond$ \hspace{1em}]
\titlecontents*{paragraph}[7.5pc]
    {\contentsmargin{6.5pc}\itshape}
    {\thecontentslabel}
    {}
    {\hspace{1ex} \thecontentspage}
    [\hspace{1em} $\diamond$ \hspace{1em}]
\titlecontents*{subparagraph}[7.5pc]
    {\contentsmargin{6.5pc}\itshape}
    {\thecontentslabel}
    {}
    {\hspace{1ex} \thecontentspage}
    [\hspace{1em} $\diamond$ \hspace{1em}]
\newcommand\partintroduction{
    \setcounter{secnumdepth}{0}
    \addtocontents{toc}{\protect\setcounter{toctemp}{\value{tocdepth}}
        \protect\setcounter{tocdepth}{0}}
    \section{Introduction}
    \setcounter{secnumdepth}{2}
    \addtocontents{toc}{
        \protect\setcounter{tocdepth}{\value{toctemp}}}}
\newcounter{toctemp}
\titlecontents{pchapter}[0pc]
    {\addvspace{10pt}\filright}%
    {\scshape\chaptername\ \thecontentslabel: }%
    {}%
    {\hfill{\bfseries\thecontentspage}}%
\titlecontents{psection}[2pc]%
    {\filright\small}%
    {\sectionname\ \thecontentslabel \hspace{0.8em}}%
    {}%
    {\titlerule*[.6em]{.}\hspace{0.3em}\thecontentspage}%
\titlecontents{psubsection}[6.5pc]%
    {\filright\small}%
    {\contentslabel[\thecontentslabel)]{2.5pc}}%
    {}%
    {\titlerule*[.6em]{.}\hspace{0.3em}\thecontentspage}%
\newswitch{lof}
\def\listfigurename{List of Figures}
\def\listoffigures{\@starttoc{lof}\listfigurename}
\newswitch{lot}
\def\listtablename{List of Figures}
\def\listoffigures{\@starttoc{lot}\listtablename}
\def\needref{\@ifnextchar[{\fnts@needrefopt}{\fnts@needref}}
    \def\fnts@needref{\todo[color=green]{Citation Needed}}
    \def\fnts@needrefopt[#1]{\todo[color=green, prepend, caption={
        Citation Needed:}]{#1}}
\def\addref{\todo[color=green]{Add reference}}
\def\finish{\@ifnextchar[{\fnts@finishopt}{\fnts@finish}}
    \def\fnts@finish{\todo[color=Cerulean, inline]{Finsh proof.}}
    \def\fnts@finishopt[#1]{\todo[color=Cerulean, inline]{Finsh #1.}}
\newswitch{tdo}
\def\listtodosname{Todo List}
\def\listoftodos{\@starttoc{tdo}\listtodosname}
\def\@dotsep{5}
\iffnts@monograph
\newcounter{appendix}
\newcounter{chaptertemp}
\renewcommand\theappendix{\Alph{appendix}}
\newenvironment{appendices}
    {\setcounter{chaptertemp}{\value{chapter}}
        \setcounter{chapter}{\value{appendix}}
        \let\fnts@thechapter\thechapter
        \renewcommand\thechapter{\Alph{chapter}}
        \let\f@chaptername\chaptername
        \let\chaptername\appendixname
        \addtocontents{toc}{
            \protect\renewcommand\protect\chaptername{Appendix}
            }}
    {\setcounter{appendix}{\value{chapter}}
        \setcounter{chapter}{\value{chaptertemp}}
        \let\thechapter\fnts@thechapter
        \let\appendixname\chaptername
        \let\chaptername\f@chaptername
        \addtocontents{toc}{
            \protect\renewcommand\protect\chaptername{\f@chaptername}
            }}
\fi
\iffnts@classnotes
\newcounter{lecture}[part]
\newcommand\makelecture[1][]{
    \let\@title\lecturetitle
    \let\@subtitle\@empty
    \ifx\CourseName\@empty\else
        \let\@subtitle\lecturesubtitle
    \fi
    \amsartmaketitle}
\def\lecturetitle{Lecture Notes}
\def\lecturesubtitle{for \CourseName}
\def\fnts@includelecture[#1]{
    \filbreak
    \ifx\fnts@temp#1\fnts@temp
        \stepcounter{lecture}
    \else
        \setcounter{lecture}{#1}
    \fi
    \marginnote{Lecture \thelecture\\ \@date}
        [0.25\baselineskip]
    \setlength{\marginparwidth}{1in}
    \reversemarginpar\marginnote{\fnts@guestauthor}
        [0.75\baselineskip]
    \fnts@lecturesep}
\let\fnts@guestauthor\@empty
\def\fnts@lecturesep{
    \noindent\parbox[t][1\baselineskip][c]{\linewidth}{
        \hfill \fnts@lecturefill \hspace*{\fill}}
    \vspace{0.67\baselineskip}}
\def\fnts@lecturefill{\fnts@repeat 7 \fnts@lsym}
\def\fnts@lsym{\fnts@lsymsep $\star$ \fnts@lsymsep}
\def\fnts@lsymsep{\hspace{.5pc}}
\def\fnts@repeat#1#2{
    \count0=#1 \loop \ifnum\count0>0 \advance\count0 by -1 #2\repeat}
\newcommand\lecture[2]{
    \ifx#1\subfile
        \let\author\fnts@lectureauthor
        \def\makelecture{\@testopt{\fnts@includelecture}{}}
        #1{#2}
    \else
        \ClassError{fillsnts}{
            The first argument of \protect\lecture\space must be
                \protect\subfile.}{}
    \fi}
\def\fnts@lectureauthor#1{\def\fnts@guestauthor{#1}}
\newcounter{assignment}[part]
\providecommand\assignmenttitle[1]{\def\@title{#1}}
\providecommand\assignmentauthor[1]{\def\@author{#1}}
\providecommand\inputassignment[1]{
    \cleardoublepage
    \let\title\assignmenttitle
    \renewcommand\fnts@assignment{\fnts@assignmentinclude}
    \subfile{#1} \let\title\relax \let\author\relax
    \cleardoublepage}
\def\fnts@assignment{
    \thispagestyle{assignment}
    \def\fnts@assign@author{
        \makebox[\fnts@ila][c]{
            {\@authors} \hfill \textbar \hfill {\small\itshape \@date}
        }}}
\def\fnts@assignmentinclude{
    \thispagestyle{headings}
    \def\fnts@assign@author{
        \makebox[\fnts@ila][c]{
            {\small\itshape \@date}
        }}
    \setcounter{problem}{0}
    \renewcommand\theproblem{\theassignment-\arabic{problem}}}
\newcommand\assignment[2]{
    \ifx#1\subfile
        \cleardoublepage
        \let\title\assignmenttitle
        \renewcommand\fnts@assignment{\fnts@assignmentinclude}
        #1{#2}
        \let\title\relax \let\author\relax
        \cleardoublepage
    \else
        \ClassError{fillsnts}{
            The first argument of \protect\lecture\space must be
                \protect\subfile.}{}
    \fi}
\newcommand\makeassignment[1][]{
    \ifx\fnts@temp#1\fnts@temp
        \stepcounter{assignment}
    \else
        \setcounter{assighment}{#1}
    \fi
    \fnts@assignment
    \def\fnts@assign@titleformat{\LARGE\bfseries\@title}
    \settowidth\fnts@ila{\fnts@assign@titleformat}
    \begingroup
    \bigskip
        \begin{flushright}
            \hfill {\fnts@assign@titleformat} \par \rule{1\fnts@ila}{0.5pt} \par
            \fnts@assign@author
        \end{flushright}
        \vspace{2\baselineskip}
    \endgroup}
\newcommand\supnote[2]{
    \ifx#1\subfile
    \cleardoublepage
    #1{#2}
    \let\title\relax \let\author\relax
    \cleardoublepage
    \else
    \ClassError{fillsnts}{
        The first argument of \protect\supnote\space must be
        \protect\subfile.}{}
    \fi}
\newenvironment{coursenotes}[1]
{\part{\CourseName}
    \startcontents[part\thepart]
    \renewcommand\contentsname{#1}
    \cleardoublepage}
{\cleardoublepage
    \stopcontents[part\thepart]
}
\newenvironment{courseassignments}[1]
{\part{\CourseName}
    \let\oldinputlecture\inputlecture
    \let\inputlecture\relax
    \startcontents[part\thepart]
    \renewcommand\contentsname{#1}
    \cleardoublepage}
{\cleardoublepage
    \let\inputlecture\oldinputlecture
    \stopcontents[part\thepart]}
\newenvironment{courselectures}[1]
{\part{\CourseName}
    \let\oldinputassignment\inputassignment
    \let\inputassignment\relax
    \startcontents[part\thepart]
    \renewcommand\contentsname{#1}
    \cleardoublepage}
{\cleardoublepage
    \let\inputassignment\oldinputassignment
    \stopcontents[part\thepart]}
\fi
\@ifundefined{chapter}
    {\newaliascnt{thmparent}{section}}
    {\newaliascnt{thmparent}{chapter}}
\declaretheoremstyle[
    numbered=yes,
    preheadhook=\bleftbar,
    prefoothook=\eleftbar,
    headindent=0pt,
    headformat=\NUMBER\hspace*{0.7ex} \NAME\ \NOTE,
    headfont=\bfseries,
    headpunct=,
    notebraces={(}{)},
    notefont=\normalfont,
    postheadspace=1ex,
    bodyfont=\normalfont,
    ]{ftheorem}
\declaretheorem[style=ftheorem, parent=thmparent,
    refname={theorem, theorems},
    Refname={Theorem, Theorems}]{theorem}
\declaretheorem[style=ftheorem, sibling=theorem,
    refname={proposition, propositions},
    Refname={Proposition, Propositions}]{proposition}
\declaretheorem[style=ftheorem, sibling=theorem,
    refname={lemma, lemmas},
    Refname={Lemma, Lemmas}]{lemma}
\declaretheorem[style=ftheorem, sibling=theorem,
    refname={corollary, corollaries},
    Refname={Corollary, Corollaries}]{corollary}
\declaretheoremstyle[
    spaceabove=\baselineskip, spacebelow=\baselineskip,
    headindent=0pt,
    headformat=\NUMBER\hspace*{0.7ex} \NAME\ \NOTE,
    headfont=\bfseries,
    headpunct=,
    notebraces={\textup(}{\textup)},
    notefont=\normalfont\itshape,
    postheadspace=2ex,
    bodyfont=\normalfont,
    qed=$\bullet$
    ]{fexample}
\declaretheorem[style=fexample, numbered=yes, sibling=theorem,
    refname={example, examples},
    Refname={Example, Examples}]{example}
\declaretheoremstyle[
    preheadhook=\bframed,
    prefoothook=\eframed,
    headindent=0pt,
    headformat=\NAME \NOTE,
    headfont=\scshape,
    headpunct=.,
    notebraces={(}{)},
    notefont=\normalfont,
    postheadspace=0.5em,
    bodyfont=\normalfont,
    ]{fdefinition}
\declaretheorem[style=fdefinition, numbered=no,
    refname={definition, definitions},
    Refname={Definition, Definitions}]{definition}
\declaretheoremstyle[
    preheadhook={\addtolength{\leftmargin}{30pt}
                 \addtolength{\linewidth}{-45pt}
                 \parshape 1 30pt \linewidth},
    headindent=0pt,
    headformat=\NAME\, \NUMBER\unskip \NOTE,
    headfont=\scshape,
    headpunct=:,
    notebraces={(}{)},
    notefont=\normalfont,
    postheadspace=0.5em,
    bodyfont=\normalfont,
    ]{fclaim}
\declaretheorem[style=fclaim, numbered=no]{claim}
\declaretheorem[style=fclaim, numbered=no]{remark}
\declaretheorem[style=fclaim, numbered=no,
    preheadhook={\small}]{note}
\declaretheorem[style=fclaim, numbered=yes, numberwithin=theorem]{step}
\renewcommand*{\thestep}{\Roman{step}}
\def\fm@exoptqed{}
\iffcls@end
    \renewcommand\fm@exoptqed[1]{#1}
\else
    \renewcommand\fm@exoptqed[1]{}
\fi
\newlength\fnts@prbindent
\setlength\fnts@prbindent{\textwidth}
\addtolength\fnts@prbindent{-1em}
\declaretheoremstyle[
    numbered=yes,
    spacebelow=1\medskipamount,
    spaceabove=1\medskipamount,
    preheadhook={\small\parshape 2 0pt \linewidth 1em \textwidth},
    headindent=0pt,
    headfont=\bfseries,
    headformat=\NAME\, \NUMBER \NOTE,
    headpunct=:\newline,
    notebraces={(}{)},
    notefont=\normalfont,
    postheadspace=0em,
    bodyfont=\normalfont,
    ]{fproblem}
\newlength\fnts@exerindent
\setlength\fnts@exerindent{2em}
\declaretheoremstyle[
    numbered=yes,
    spacebelow=1\bigskipamount,
    spaceabove=1\bigskipamount,
    preheadhook={\small},%\parshape 2 0pt \linewidth 1em \fnts@prbindent},
    headindent=0pt,
    headfont=\bfseries,
    headformat={\makebox[\fnts@exerindent][l]{\NUMBER.}\NOTE},
    headpunct=,
    notebraces={\unskip(}{) }, % this is an ad-hoc fix for spacing issues
    notefont=\normalfont,
    postheadspace=0em,
    bodyfont=\normalfont,
    postheadhook=\hangindent=\fnts@exerindent,
    ]{fexergrp}
\declaretheorem[
    style=fproblem, parent=thmparent,
    refname={problem,problems},
    Refname={Problem,Problems}]{problem}
\renewcommand{\theproblem}{\arabic{problem}}
\declaretheorem[
    style=fproblem, parent=thmparent,
    refname={exercise,exercises},
    Refname={Exercise,Exercises}]{exercise}
\renewcommand{\theexercise}{\arabic{exercise}}
\declaretheorem[
    style=fproblem, parent=thmparent,
    refname={exercise,exercises},
    Refname={Exercise,Exercises}]{f@exer}
\renewcommand{\thef@exer}{\arabic{f@exer}}
\declaretheorem[
    style=fexergrp, parent=thmparent,
    refname={exercise,exercises},
    Refname={Exercise,Exercises}]{f@exergrp}
\renewcommand{\thef@exergrp}{\arabic{f@exergrp}}
\newenvironment{exercises}%
   {\begingroup
        \LARGE\bfseries
        Exercises
    \endgroup
    \begin{exercises*}}
   {\end{exercises*}}
\newenvironment{exercises*}{
    \let\exercise\f@exergrp
    \let\endexercise\endf@exergrp
    }{
    \let\exercise\f@exer
    \let\endexercise\endf@exer
    }
\def\fm@blackqed{\ensuremath{\rule[1pt]{1.6ex}{1.6ex}}}
\def\fm@whiteqed{\ensuremath\square}
\def\fm@gap{}
\iffcls@noflush\else % set to true by the |noflush| option
    \renewcommand\fm@gap{\hbox{}\nobreak\hfill}
\fi
\renewrobustcmd\qed{%
    \ifmmode    \mathqed
    \else
        \leavevmode\unskip\nobreak  \fm@gap
        \quad\hbox{\qedsymbol}%
    \fi
    }
\iffcls@prftombstone % set to true by the |empty| option
    \renewcommand\qedsymbol\fm@whiteqed
\else
    \renewcommand\qedsymbol\fm@blackqed
\fi
\newenvironment{stepproof}
   {\addtolength{\leftmargin}{1cm}
    \addtolength{\linewidth}{-1.5cm}
    \parshape 1 1cm \linewidth
    \begin{proof}[Proof of step]\renewcommand{\qedsymbol}{$\square$}}
   {\end{proof}}
\newenvironment{claimproof}
   {\addtolength\leftmargin{1cm}
    \addtolength\linewidth{-1.5cm}
    \parshape 1 1cm \linewidth
    \begin{proof}[Proof of claim]\renewcommand{\qedsymbol}{$\square$}}
   {\end{proof}}
\begingroup
    \catcode`|=0 \catcode`[=1 \catcode`]=2
    \catcode`\{=12 \catcode`\}=12 \catcode`\\=12
    |gdef|fcreate@suppressed#1[
        |long|csdef[suppressed@#1] ##1\end{#1}[|end[#1]]]
|endgroup % we have to end the group with the modified catcodes
\def\fcreate@prooflike#1#2#3{
    \renewcommand\qedsymbol{#3}
    \newenvironment{#1}[1][#2]{
        \pushQED{\qed}
        \normalfont \topsep6\p@\@plus6\p@\relax
        \trivlist
        \item[\hskip\labelsep\itshape##1\@addpunct{.}]\ignorespaces
    }{
        \popQED\endtrivlist\@endpefalse
    }
    \fcreate@suppressed{#1}
    \csletcs{original@#1}{#1}
    \csdef{invisible#1s}{
        \csdef{#1}{
            \catcode`\{=12 \catcode`\}=12 \catcode`\\=12
            \csuse{suppressed@#1}}
    }
    \csdef{visible#1s}{
        \csletcs{#1}{original@#1}
    }
}
\providecommand\proofname{Proof}
\newcommand\answerqed{}     \let\answerqed\fm@wblackqed
\fcreate@prooflike{answer}{\answername}{\answerqed}
\providecommand\answername{Answer}
\newcommand\solutionqed{}   \let\solutionqed\fm@blackqed
\fcreate@prooflike{solution}{\solutionname}{\solutionqed}
\providecommand\solutionname{Solution}
\newcommand\outlineqed{}    \let\outlineqed\fm@whiteqed
\fcreate@prooflike{outline}{\outlinename}{\outlineqed}
\providecommand\outlinename{Outline}
\let\oldphi\phi
\let\phi\varphi
\let\varphi\oldphi
\providecommand\defn[1]{\textbf{#1}}
\def\bigdefn@font{\normalfont\SMALL\bfseries}
\providecommand\bigdefn[1]{\defn{#1}%
    \marginnote{\bigdefn@font#1}}
\newcommand\qtext[1]{\quad\text{#1}}
\newcommand\qqtext[1]{\qquad\text{#1}}
\newcommand\qand{\qtext{and}}
\newcommand\qqand{\qqtext{and}}
\let\mathand\qand
\let\mathaand\qqand
\providecommand\dbend{}
\renewcommand\dbend{
    \marginnote{\resizebox{!}{3em}{
        \tikz[
            line cap=but,
            line join=round,
            x=.5em,
            thick,
            y=1*(height("Z")-\pgflinewidth)*(1-sin(10)),
            rotate=-10,
            rounded corners=1.5pt]
        \draw (1, 0) -- (0, 0) -- (1, 1) -- (0, 1);
    }\hspace*{2em}}[-1.5em]}
\newcommand\defeq{\coloneqq}
\let\deq\defeq
\newcommand\eqdef{\eqqcolon}
\let\eqd\eqdef
\newcommand\maps[1][]{\stackrel{#1}{\fm@maparrow}}
\def\fm@maparrow{\rightarrow}
\newcommand\mapsfrom{\mathrel{\reflectbox{\ensuremath{\mapsto}}}}
\newcommand\mapping{
    \@ifstar{\fm@smapping}{\fm@mapping}}
\def\fm@smapping{\@testopt\fm@smappingi{}}
\def\fm@smappingi[#1]#2{\fm@mappingiii[#1]#2.}
\def\fm@mapping{\@testopt\fm@mappingi{}}
\def\fm@mappingi[#1]#2{\@testopt{\fm@mappingii[#1]{#2}}{}}
\def\fm@mappingii[#1]#2[#3]{#1\colon\fm@mappingiii[#3]#2.}
\def\fm@mappingiii[#1]#2, #3.{#2 \maps[#1] #3}
\newcommand\into{\def\fm@maparrow{\hookrightarrow} \maps}
\newcommand\onto{\def\fm@maparrow{\twoheadrightarrow} \maps}
\newcommand\mapinto{\def\fm@maparrow{\hookrightarrow} \mapping}
    \let\inject\mapinto
\newcommand\maponto{\def\fm@maparrow{\twoheadrightarrow} \mapping}
    \let\surject\maponto
\providecommand\inv{^{-1}}
\providecommand\pull{^{\ast}}
\providecommand\push{_{\ast}}
\newcommand\bb[1]{\ensuremath{\mathbb{#1}}}
\newcommand\bbN[1][]{{\ensuremath{{{\bb N}^{#1}}}}}
\newcommand\bbZ[1][]{{\ensuremath{{{\bb Z}^{#1}}}}}
\newcommand\bbQ[1][]{{\ensuremath{{{\bb Q}^{#1}}}}}
\newcommand\bbR[1][]{{\ensuremath{{{\bb R}^{#1}}}}}
\newcommand\bbC[1][]{{\ensuremath{{{\bb C}^{#1}}}}}
\newcommand\bbH[1][]{{\ensuremath{{{\bb H}^{#1}}}}}
\newcommand\bbRP[1][]{{\ensuremath{{{\bb{RP}}^{#1}}}}}
\newcommand\bbCP[1][]{{\ensuremath{{{\bb{CP}}^{#1}}}}}
\newcommand\bbT[1][]{{\ensuremath{{{\bb T}^{#1}}}}}
\newcommand\bbK[1][]{{\ensuremath{{{\bb K}^{#1}}}}}
\newcommand\cind{\bbN}
    \let\cindex\cind
\newcommand\aind[1][A]{\ensuremath{{\bb #1}}}
    \let\aindex\aind
\newcommand\Cat[1]{\textbf{#1}}
\let\mathcat\Cat
\let\mcat\Cat
\newcommand\Ob{\@ifstar{\fm@sOb}{\fm@Ob}}
\def\fm@sOb#1{\operatorname{Ob} {#1}}
\def\fm@Ob#1{\ensuremath{\operatorname{Ob} {\Cat{#1}}}}
\def\fm@homname{Hom}
\providecommand\Hom{}
\renewcommand\Hom[2][]{\ensuremath{\fm@hom[\Cat{#1}]#2.}}
\providecommand\hom{}
\renewcommand\hom[2][]{\ensuremath{\fm@hom[#1]#2.}}
\def\fm@hom[#1]#2, #3.{\ensuremath{\operatorname{\fm@homname}_{#1} (#2, #3)}}
\def\fm@endname{End}
\providecommand\Endo{}
\renewcommand\Endo[2][]{\ensuremath{\fm@endo[\Cat{#1}]#2.}}
\def\endo{\@testopt\fm@endoi{}}
\def\fm@endoi[#1]#2{\ensuremath{\fm@endo[#1]#2.}}
\def\fm@endo[#1]#2.{\ensuremath{\operatorname{\fm@endname}_{#1} (#2)}}
\let\End\endo
\newcommand\Aut[2][]{\def\fm@endname{Aut} \Endo[#1]{#2}}
\newcommand\aut[2][]{\def\fm@endname{Aut} \endo[#1]{#2}}
\newcommand\id[1][]{\operatorname{id}_{#1}}
\newcommand\iso{\simeq}
\newcommand\spn[1]{\operatorname{span}\{#1\}}
\let\fm@dualsymbol\ast
\newcommand\dualsymbol[1]{\let\fm@dualsymbol#1}
\newcommand\dual{\ensuremath{^{\fm@dualsymbol}}}
\newcommand\img{\operatorname{im}}
\newcommand\coker{\operatorname{coker}}
\newcommand\trace{\operatorname{tr}}
\newcommand\trans{^\text{T}}
\newcommand\adj{^{\dagger}}
\newcommand\clsr{\@ifstar{\fm@sclsr}{\fm@clsr}}
\def\fm@sclsr#1{\overline{#1}}
\def\fm@clsr#1{\operatorname{cl} #1}
\newcommand\cls[1]{\overline{#1}}
\newcommand\Cls{\operatorname{cl}}
\newcommand\intr{\@ifstar{\fm@sintr}{\fm@intr}}
\def\fm@sintr#1{\mathring{#1}}
\def\fm@intr#1{\operatorname{int} #1}
\newcommand\Intr{\operatorname{int}}
\providecommand{\trsver}{\mathrel{\text{\tpitchfork}}}
\def\tpitchfork{
    \vbox{
        \baselineskip\z@skip
        \lineskip-.52ex
        \lineskiplimit\maxdimen
        \m@th
        \ialign{##\crcr\hidewidth\smash{$-$}\hidewidth\crcr$\pitchfork$\crcr}}}
\providecommand\dif{\, d}
\DeclarePairedDelimiter\abs{\lvert}{\rvert}
\DeclarePairedDelimiter\norm{\lVert}{\rVert}
\newcommand\inner{\@ifstar{\fm@sinner}{\fm@inner}}
\def\fm@sinner#1{\fm@sinneri#1.}
\def\fm@sinneri#1, #2.{\fm@inneriii*{#1\fm@innersep#2}}
\def\fm@inner{\@testopt\fm@inneri{}}
\def\fm@inneri[#1]#2{\fm@innerii[#1]#2.}
\def\fm@innerii[#1]#2, #3.{\fm@inneriii[#1]{#2\fm@innersep#3}}
\DeclarePairedDelimiter\fm@inneriii{\langle}{\rangle}
\def\fm@innersep{,}

\newcommand\seq[1]{\ensuremath{\fm@seq#1.}}
\def\fm@seq#1_#2.{\{{#1}_{#2}\}}

\newcommand\supp{\operatorname{supp}}
\def\refname{References}
\def\bibname{Bibliography}
\def\@defaultbiblabelstyle#1{#1.}
\def\bibliographystyle#1{%
    \if@filesw\immediate\write\@auxout{\string\bibstyle{#1}}\fi%
    \def\@tempa{#1}%
    \def\@tempb{amsplain}%
    \def\@tempc{}%
    \ifx\@tempa\@tempb
        \def\@biblabel##1{\@defaultbiblabelstyle{##1}}%
        \def\bibsetup{}%
    \else
        \def\bibsetup{\labelsep6\p@}%
        \ifx\@tempa\@tempc
            \def\@biblabel##1{}%
            \def\bibsetup{\labelwidth\z@ \leftmargin24\p@
                \itemindent-\leftmargin
                \labelsep\z@ }%
        \fi
    \fi}
\providecommand\bibliofont{}
\renewcommand\bibliofont{\footnotesize}
\providecommand\@bibtitlestyle{}
\renewcommand\@bibtitlestyle{}
\newenvironment{thebibliography}[1]
    {\@bibtitlestyle
        \normalfont\bibliofont\labelsep .5em\relax
        \renewcommand\theenumiv{\@arabic\c@enimiv}
        \let\p@enumiv\@empty
        \list{\@biblabel{\theenumiv}}
            {\settowidth\labelwidth{\@biblabel{#1}}
            \leftmargin\labelwidth \advance\leftmargin\labelsep
            \@openbib@code \usecounter{enumiv}}
        \sloppy \clubpenalty\@M \widowpenalty\clubpenalty
        \sfcode'\.=\@m}
    {\def\@noitemerr{\@latex@warning{Empty 'thebibliography' environment}}
        \endlist}
\let\@openbib@code\@empty
\def\bysame{\leavemode\hbox to3em{\hrulefill}\thinspace}
\providecommand\MR{}
\renewcommand\MR[1]{\relax\ifhmode\unskip\spacefactor3000 \space\fi
    MR~\MRhref{#1}{#1}}
\let\MRhref\@gobble
\providecommand\URL{}
\renewcommand\URL{\begingroup \def\@sverb##1{
        \def\@tempa####1##1{\@URL{####1}\egroup\endgroup}
        \@tempa}
    \verb}
\let\URLhref\@gobble
\def\@URL#1{\URLhref{#1}#1}
\def\indexname{Index}
\providecommand\indexfont{}
\renewcommand\indexfont{\footnotesize}
\providecommand\@indextitlestyle{}
\renewcommand\@indextitlestyle{}
\newenvironment{theindex}
    {\@restonecoltrue\if@twocolumn\@restonecolfalse\fi
        \columnseprule\z@ \columnsep 35\p@
        \@indextitlestyle
        \thispagestyle{plain}
        \let\item\@idxitem
        \parindent\z@ \parskip\z@\@plus.3\p@\relax
        \raggedright
        \hyphenpenalty\@M
        \indexfont}
    {\if@restonecol\oncolumn\else\clearpage\fi}
    \def\@idxitem{\par\hangindent 2em}
    \def\subitem{\par\hangindent 2em\hspace*{1em}}
    \def\subsubitem{\par\hangindent 3em\hspace*{2em}}
    \def\indexspace{\par\bigskip}
\newdimen\fcls@widthleft
\newdimen\fcls@widthright
\newdimen\fcls@topmargin
\def\calclayout{
    \f@tempdima\paperwidth
    \advance\f@tempdima -1\textwidth
    \fcls@widthright\f@tempdima
    \advance\f@tempdima -1\marginparsep
    \advance\f@tempdima -1\marginparwidth
    \fcls@widthleft=0.75\f@tempdima
    \advance\fcls@widthright -\fcls@widthleft
    \fcls@topmargin\paperheight
    \advance\fcls@topmargin -\textheight
    \advance\fcls@topmargin -\headheight
    \advance\fcls@topmargin -\headsep
    \divide\fcls@topmargin\tw@}
\calclayout
\geometry{
    left=1\fcls@widthleft, right=1\fcls@widthright, top=1\fcls@topmargin}
\pagenumbering{arabic}
\pagestyle{headings}
\thispagestyle{plain}
%% 
%%
%% End of file `fillsntsx.cls'.
