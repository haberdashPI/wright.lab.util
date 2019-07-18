\name{guess.experiment.filenames}
\alias{guess.experiment.filenames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Guessing Experiment File Organization
}
\description{
Guess the organization of files for a given experiment.  This
guessworks is based on naming conventions used throughout old
programs run in the lab. Each file is a single block of an
expeirment, and indicates the subject id, date, and task for the
block.
}
\usage{
guess.experiment.filenames(dir, file.pattern = old.file.pattern, default.month.codes = month.codes, subject.month.codes = list(), label.post.test = NULL,verified.ids = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{dir}{ the directory containing all files form a single expeirment}
    \item{file.pattern}{ The naming convention used by files.
        There are two predefined patterns, old.file.pattern and
        new.file.pattern. Old files types start with letters
        and new files start with numbers. You can make your
        own file patterns. Refer to file.patterns.R for more information. }
   \item{default.month.codes}{the default month code used to order files
        month codes can be NULL, in which case files are sorted by
        their date modified. The month codes translate strings
        into an unambiguous format so that sorting by the date
        in the file name will correctly order files. There are
        two sets of pre-made month codes: month.codes and late.month.codes
        month.codes should work for most files but if there are
        late months that have been ambiguously coded, this will fail.
        For subject with ambiguous late months you should use
        set subject specific month code. See the file.patterns.R
        file for more information on making your own month codes.}
  \item{subject.month.codes}{ provdies subject specific codes for converting
        months to an unambiguous format. Each item in the list
        has the name of a subject, and a value with month
        codes used to convert dates into sortable format for
        the given subejct.}
      \item{label.post.test}{If non-NULL this will cause the last day
        for each participant to be set to its value. Thus all post
        tests will have the same value, and can be easilly selected.
        This is useful to set when some participants practice a different
        number of days than others.}
      \item{verified.ids}{
        Beause dates are ambiguously coded in many cases, it is
        important to verify that they are \emph{actually} in the correct
        order. For each subject not added to this list, a warning will
        be printed asking you to verify that the order is correct. Once
        you have verified a given subject, add their id to this list.
        BUT MAKE SURE THE ID IS UNIQUE. Otherwise if you add new
        subjects there will be no warning message to alert you to
        the possibility that dates are our of order.

        If a subject's days are not ordered correctly you have several
        options, listed below from easiest to hardest.
        
        1. You can specify a different set of month codes to use
          for that subject. Often you just need to give them the
          late.month.codes instead of month.codes. 
        2. You can use the date modified of each file to order
          days. You can do this by specifying a file pattern with 'day'
          set to NULL. 

        3. You can also design your own month codes (see
           file.patterns.R) for more information.

        4. You can re-order the days for this subject in the
          returned data table from this function. 
      }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data set that contains a row for each file. The columns
indicate the subject, task and day for that file. You can use
find.file.deltas to determine the threshold for each data file
in this data set.
}
\author{
David Little
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
