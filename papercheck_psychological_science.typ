// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: white, width: 100%, inset: 8pt, body))
      }
    )
}

//#assert(sys.version.at(1) >= 11 or sys.version.at(0) > 0, message: "This template requires Typst Version 0.11.0 or higher. The version of Quarto you are using uses Typst version is " + str(sys.version.at(0)) + "." + str(sys.version.at(1)) + "." + str(sys.version.at(2)) + ". You will need to upgrade to Quarto 1.5 or higher to use apaquarto-typst.")

// counts how many appendixes there are
#let appendixcounter = counter("appendix")
// make latex logo
// https://github.com/typst/typst/discussions/1732#discussioncomment-11286036
#let TeX = {
  set text(font: "New Computer Modern",)
  let t = "T"
  let e = text(baseline: 0.22em, "E")
  let x = "X"
  box(t + h(-0.14em) + e + h(-0.14em) + x)
}

#let LaTeX = {
  set text(font: "New Computer Modern")
  let l = "L"
  let a = text(baseline: -0.35em, size: 0.66em, "A")
  box(l + h(-0.32em) + a + h(-0.13em) + TeX)
}

#let firstlineindent=0.5in

// documentmode: man
#let man(
  title: none,
  runninghead: none,
  margin: (x: 1in, y: 1in),
  paper: "us-letter",
  font: ("Times", "Times New Roman"),
  fontsize: 12pt,
  leading: 18pt,
  spacing: 18pt,
  firstlineindent: 0.5in,
  toc: false,
  lang: "en",
  cols: 1,
  numbersections: false,
  numberdepth: 3,
  first-page: 1,
  suppresstitlepage: false,
  doc,
) = {

  if suppresstitlepage {counter(page).update(first-page)}

  set page(
    margin: margin,
    paper: paper,
    header-ascent: 50%,
    header: grid(
      columns: (9fr, 1fr),
      align(left)[#upper[#runninghead]],
      align(right)[#context counter(page).display()]
    )
  )
  

  

 

  set table(    
    stroke: (x, y) => (
        top: if y <= 1 { 0.5pt } else { 0pt },
        bottom: .5pt,
      )
  )

  set par(
    justify: false, 
    leading: leading,
    first-line-indent: firstlineindent
  )

  // Also "leading" space between paragraphs
  set block(spacing: spacing, above: spacing, below: spacing)

  set text(
    font: font,
    size: fontsize,
    lang: lang
  )
  
  show link: set text(blue)
  show "al.'s": "al.\u{2019}s"

  show quote: set pad(x: 0.5in)
  show quote: set par(leading: leading)
  show quote: set block(spacing: spacing, above: spacing, below: spacing)
  // show LaTeX
  show "TeX": TeX
  show "LaTeX": LaTeX

  // format figure captions
  show figure.where(kind: "quarto-float-fig"): it => block(width: 100%, breakable: false)[
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2, outlined: false)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2, outlined: false)[#it.supplement #it.counter.display()]
    ]
    #align(left)[#par[#emph[#it.caption.body]]]
    #align(center)[#it.body]
  ]
  
  // format table captions
  show figure.where(kind: "quarto-float-tbl"): it => block(width: 100%, breakable: false)[#align(left)[
  
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2, outlined: false, numbering: none)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2, outlined: false, numbering: none)[#it.supplement #it.counter.display()]
    ]
    #par[#emph[#it.caption.body]]
    #block[#it.body]
  ]]
  
    set heading(numbering: "1.1")
    
    show heading: set text(size: fontsize)


 // Redefine headings up to level 5 
  show heading.where(
    level: 1
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(center)
    #if(numbersections and it.outlined and numberdepth > 0 and counter(heading).get().at(0) > 0) [#counter(heading).display()] #it.body
  ]
  
  show heading.where(
    level: 2
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #if(numbersections and it.outlined and numberdepth > 1 and counter(heading).get().at(0) > 0) [#counter(heading).display()] #it.body
  ]
  
  show heading.where(
    level: 3
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #set text(style: "italic")
    #if(numbersections and it.outlined and numberdepth > 2 and counter(heading).get().at(0) > 0) [#counter(heading).display()] #it.body
  ]

  show heading.where(
    level: 4
  ): it => text(
    weight: "bold",
    it.body
  )

  show heading.where(
    level: 5
  ): it => text(
    weight: "bold",
    style: "italic",
    it.body
  )
  
  

  if cols == 1 {
    doc
  } else {
    columns(cols, gutter: 4%, doc)
  }
  



}


#show: document => man(
  runninghead: "PAPERCHECK FOR PSYCHOLOGICAL SCIENCE",
  numberdepth: 3,
  document,
)

\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Papercheck for Psychological Science
]
)
]
#set align(center)
#block[
\
Lisa DeBruine#super[1];, Daniel Lakens#super[2];, and Cristian Mesquida#super[2]

#super[1];Institute of Neuroscience and Psychology, University of Glasgow

#super[2];Industrial Engineering and Innovation Sciences, Eindhoven University of Technology

]
#set align(left)
\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Author Note
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Lisa DeBruine #box(image("_extensions/wjschne/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) #link("https://orcid.org/0000-0002-7523-5539")

Daniel Lakens #box(image("_extensions/wjschne/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) #link("https://orcid.org/0000-0002-8393-5316")

Cristian Mesquida #box(image("_extensions/wjschne/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) #link("https://orcid.org/0000-0002-1542-8355")

A reproducible version of this manuscript is available at XXX. The authors have no conflict of interest to declare.

Correspondence concerning this article should be addressed to Daniel Lakens, Industrial Engineering and Innovation Sciences, Eindhoven University of Technology, Den Dolech 2, 5600 MB, Eindhoven, Eindhoven, Email: #link("mailto:D.Lakens@tue.nl")[D.Lakens\@tue.nl]

#pagebreak()

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Abstract
]
)
]
#block[
Papercheck is cool.

]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
#emph[Keywords];: Peer Review, Quality Control, Automation, Open Data

#pagebreak()

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Papercheck for Psychological Science
]
)
]
== Abstract
<abstract>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Metascientific research has revealed that researchers often fall short in implementing best research practices when designing studies, reporting findings, citing the literature, or sharing data and code. Researchers have time constraints that limit the extent to which they can master all aspects of the research process, and cognitive constraints that limit which practices they remember to implement in their work. By capturing expert knowledge within an automated tool, we aim to elevate the quality of scientific research and disseminate best practices more widely. We illustrate how best practices can be automatically detected by analyzing papers published in Psychological Science in the last decade. Researchers can use the tool before submitting or when reviewing scientific articles. Our module based software tool can easily be extended by experts in the scientific community to automatically check best practices, educate scientists, and increase the quality of scientific reporting.

== Introduction
<introduction>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Metascientific research demonstrates significant room for improvement in how scientists design studies, report findings, and implement open science practices when sharing their work. The uptake of best practices is slow (#link(<ref-sharpe2013resistance>)[Sharpe, 2013];), and often suboptimal. For example, researchers often use invalid and unreliable measures (#link(<ref-goos2024assessing>)[Goos et al., 2024];), misinterpret non-significant results (#link(<ref-aczel2018quantifying>)[Aczel et al., 2018];), write insufficiently specific preregistrations (#link(<ref-van2024potential>)[Akker et al., 2024];), make mistakes in power analyses (#link(<ref-thibault2024evaluation>)[Thibault et al., 2024];), and misuse Bayes factors (#link(<ref-tendeiro2024diagnosing>)[Tendeiro et al., 2024];). Although it is the responsibility of scientists to keep up to date on best practices, time constraints and the rapid pace of new developments make this challenging. Peer reviewers, similarly pressed for time, may not identify the absence of best practices. Checklists could potentially increase the adoption of best practices, but in practice they have limited impact, as researchers might not be aware of the existence of checklists, and they still need sufficient expertise to evaluate whether they have adhered to items on the checklist (#link(<ref-dexter2017narrative>)[Dexter & Shafer, 2017];).

Human factors research suggests that automation (where researchers are kept in the loop) can offer a partial solution. Two recent developments have increased the possibility that best practices can be automatically detected. First, metascientific research has identified where the adoption of best practices is lagging, and offered concrete suggestions for improvement. This work often includes data that can be used as ground truth to validate automated checks. Second, advances in machine learning and artificial intelligence have made it easier to deal with written text, both when turning scientific articles into structured data files (e.g., the GROBID machine learning library), as when extracting or classifying information in written text. This has made it possible to develop algorithms that automatically emulate what experts do when they examine if best practices are present or absent. For example, a reviewer might search for the section in the paper that describes an a-priori power analysis, and then check if people follow best practices, and report the alpha level, desired power, the effect size metric, and a justification for the effect size, for each hypothesis tested (#link(<ref-lakens_justification_2022>)[Lakens, 2022];). If any of this information is missing, authors should be made aware that they are not following best practices, so that they can improve their paper. While many automated checks for best practices will require only simple text matching based on regular expression, more complex text extraction is possible by using large language models (LLMs).

In this article we introduce a new tool, Papercheck, which uses text search, code, and large language models to extract information from scientific text (which could be a draft of a manuscript, submitted or published articles, or preregistration documents) and provide automated suggestions for improvement. Papercheck is inspired by quality control processes in software development. For example, R package developers run a set of automated checks before they submit their package to the Comprehensive R Archive Network (CRAN). The automated checks aim to identify any remaining issues, and inform developers about how to fix them. The goal of papercheck is to screen scientific manuscripts, identify areas for improvement based on algorithms that emulate what experts would do while checking a research paper, and educate researchers on how to incorporate best practices. The tool is module based, with a dedicated module for any practice that the community wants to improve, and where automation offers a solution. This means the tool can be extended by the scientific community to fit their needs, and specialised modules can be created for different scientific fields based on what is considered a best practice in that research area. The tool can reduce the burden on peer reviewers by resolving common problems before submission, allowing peer reviewers to focus on those issues that can not be automatically detected.

Automated checks can quite accurately identify the presence or absence of information that should be reported (e.g., is the desired level of power reported in an a-priori power analysis), but a bigger challenge is to evaluate whether the provided information is valid (e.g., is there a good justification for the effect size entered in an a-priori power analysis). Papercheck focuses on the first task, and leaves the evaluation to the user. We will illustrate the potential of automated checks to improve best practices in 4 areas, namely 1) experimental design, 2) open science practices, 3) statistical inferences, and 4) reporting guidelines. Finally, we will demonstrate the usefulness of a tool that can efficiently process text in scientific articles to perform menial tasks (e.g., check if if hyperlinks are working), more easily code information from scientific articles for meta-scientific research, and incorporate existing automated checks (e.g., statcheck, checking if retracted articles are cited) in a single module-based workflow that can be expanded by the scientific community.

\#The two intended use-cases of Papercheck

The current version of Papercheck has two contexts of use. The first use-case is by scientists - either authors or reviewers - who have the goal to improve a scientific manuscript by automatically checking the presence or absence of specific best practices. These users perform this automated check because the tool is able to provide recommendations that improve the manuscript in situations where 1) a best practice is not sufficiently salient to the scientist to be noticed without assistance, 2) the scientist is not yet able to identify the absence of best practices manually due to a lack of knowledge or training, and 3) scientists do not have time to identify the absence of best practices manually. Scientists using Papercheck are aware that all modules make recommendations that can be Type 1 and Type 2 errors. This means that it is possible that many possibly problematic practices are automatically identified, while none are actually problematic upon closer inspection. It is also possible that no problematic practices are found, but many are present.

As developers, our priority is to develop modules with error rates that are low enough to not impact the usability of Papercheck for individual authors, which - following the ISO 9241-11 definition - means "the extent to which a product can be used by specified users to achieve specified goals with effectiveness, efficiency, and satisfaction in a specified context of use". Our working assumption is that preparing a scientific manuscript takes a considerable amount of time, and that scientists care about the quality of their work. Under this assumption, we believe that a tool with high error rates - say up to 50% - will be considered sufficiently useful to be adopted as long as it accurately identified some bad practices that researchers would not have identified themselves. For example, a spelling checker that marks 'Papercheck' and 'manuscirpt' as words with spelling errors, and does not mark 'extend' as a spelling error where the correct word would have been 'extent' remains useful as long as it takes relatively little time to evaluate the suggestions, even though the error rate is 2 out of 3.

The second use-case of Papercheck is as a tool to examine a large number of scientific articles. For example, a university or science funder might be interested in how many manuscripts by researchers that they employ or fund share data and code if this is in line with their policies. A meta-scientist might be interested in how many articles report effect sizes in a specific literature, and test whether there is an improvement in best practices. A scientific journal might want to evaluate how a change in editorial practices has impacted the diversity in populations that are examined in the studies they publish. Papercheck modules can easily be run on large numbers of papers, facilitating text searches and the automatic extraction and classification of information. In this use-case the usability of an automated tool requires low error rates if users want to use the output of the tool. Here, Type 2 errors are problematic, and difficult to identify, as it requires users to manually screen all papers for instances that were missed by the algorithms, and if manual screening was possible, there was no need for an automated tool. Type 1 errors might be easier to identify, but still require manually checking all instances flagged by a module. Therefore, low error rates are important for the usability of Papercheck as a meta-scientific tool. Validation procedures that quantify error rates are essential to safeguard the accuracy of results, and allow users to evaluate whether modules are accurate enough for their purposes, and communicate the error rates when presenting results. It is possible that a module has low-enough error rates for the individual use-case, while error rates are too high for meta-scientific use-cases.

= Automatically Checking Journal Article Reporting Standards
<automatically-checking-journal-article-reporting-standards>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
The journal article reporting standards (JARS) developed by the American Psychological Association (APA) offer guidance on the information that should be reported in scientific articles to enhance their scientific rigour (Appelbaum et al.~2018). Almost every published article would improve if researchers diligently followed JARS. However, the guidelines are not widely known, rarely implemented by authors, and almost never checked by editors or reviewers. Manually checking whether all reporting standards are implemented would be effortful. Automation can help by automatically checking if recommendations are followed.

The first reporting guideline that has been implemented in Papercheck is to report exact p-values. The APA Manual states:

#quote(block: true)[
Report exact p values (e.g., p = .031) to two or three decimal places. However, report p values less than .001 as p \< .001.
]

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Access to precisely reported p-values allows readers to include the test results in p value meta-analytic tests, such as p-curve, or z-curve (Simonsohn, Nelson, and Simmons 2014; Bartoš and Schimmack 2020), and makes it possible to the check the internal coherence of the reported results with tools such as Statcheck (Nuijten et al.~2015). Papercheck has a dedicated module, `exact_p`, to identify the presence of imprecise p-values. It will return all instances of imprecisely reported p-values in a paper.

We can also examine the difference between significant and non-significant effects, out of interest, and which imprecise p-values are reported. This illustrates how papercheck makes it possible to easily explore larger numbers of papers, and their properties. This allows us to uncover interesting explorative findings, such as the fact that there is a large peak at p \> 0.25, as it is apparently common for researchers to use this cut-off to indicate a non-significant result. It is unclear to us where this shared practice originates.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
The peak at 0.25 is due to the tendency of authors to treat #emph[p] values \> 0.25 as an indication of an absence of an effect. It is unclear where this criterion comes from. A possible origin might be the proposed alpha of 0.25 $lt.eq$ $alpha$ $lt.eq$ 0.35 for stopping rules in forward stepwise selection procedures (#link(<ref-bendel1977comparison>)[Bendel & Afifi, 1977];), later popularized as $alpha$ $lt.eq$ .025 in a widely used textbook on logistic regression (#link(<ref-hosmer2013applied>)[Hosmer Jr et al., 2013];), but we are unsure about this. The correct approach to conclude meaningful effects are absent is by performing an equivalence test against a smallest effect size of interest (#link(<ref-lakens_equivalence_2018>)[Lakens et al., 2018];).

== Reporting effect sizes
<reporting-effect-sizes>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
A second JARS guideline that can be automatically checked is whether people report effect sizes alongside their test result. Each test (e.g., a #emph[t];-test, #emph[F];-test, etc.) should include the corresponding effect size (e.g., a Cohen’s #emph[d];, or partial eta-squared). Based on a text search that uses regular expressions (regex), we can identify #emph[t];-tests and #emph[F];-tests that are not followed by an effect size, and warn researchers accordingly.

We have manually checked the #emph[t];-tests and #emph[F];-tests in 250 papers in Psychological Science, and our `effect_size` module detected 100% of #emph[t];-tests with effect sizes, 99% of #emph[t];-tests without effect sizes, 99% of #emph[F];-tests with effect sizes, and 100% of #emph[F];-tests without effect sizes. This is accurate enough to use the module for meta-scientific research, such as examining whether there is an increase in the best practice to report effect sizes for #emph[t];-tests and #emph[F];-tests over time. In the plot below we have run the module on 1838 published articles in the journal Psychological Science between 2014 and 2024. We can see that where a decade ago, around 25% of articles report #emph[t];-tests or #emph[F];-tests without an effect size this has dropped to between 10% and 15% a decade later. Perhaps a tool like Papercheck can reduce this percentage even further.

= Checking Data and Code in Repositories
<checking-data-and-code-in-repositories>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
There is widespread support among scientists for open science practices, such as data and code sharing (#link(<ref-ferguson2023survey>)[Ferguson et al., 2023];). There is increasing awareness of the importance of making data and code available alongside publication, and sometimes this is even a requirement by funders or journals. As data and code sharing is a relatively new practice, and many scientists lack training in open science, it is common to see badly documented data repositories. Best practices exist, such as the #link("https://www.projecttier.org/tier-protocol/protocol-4-0/")[TIER protocol];, but not all researchers might be aware of best practices. By automatically checking data repositories, Papercheck can increase awareness of best practices, and make specific suggestions for improvements.

At a minimum a data repository should contain a Read Me file with instructions for how to reproduce the results. If data is shared, it should be stored in a 'data' folder, or at least have the word 'data' in the filename. Code or scripts should similarly be shared in a folder with that name, or at least with the word in the filename. Finally, if data is shared, there should be a codebook or data dictionary that explains which variables are in the dataset in order to allow others to re-use the data. Ideally peer reviewers or editors would check the contents of a data repository. In practice, time constraints mean that data repository are often not checked by any human. Automation can perform some of the checks that peers might otherwise perform manually.

We will illustrate the process of checking a data repository by focusing on projects on the #link("www.osf.io")[Open Science Framework];. We start by searching for OSF URLs using the `osf_links` function. OSF links can be tricky to find in PDFs, since they can insert spaces in odd places, and view-only links that contain a ? are often interpreted as being split across sentences. This function is our best attempt at catching and fixing them all.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
If valid, the link is processed, and the OSF Application Programming Interface (API) is used to retrieve whether the link points to a file, project, or registration. This is achieved through the `osf_retrieve()` function. This function will retrieve all child components, files and folders if the argument `recursive = TRUE` is specified, and if the argument `find_project = TRUE` is specified, it will also look up the parent project of any links.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We can then use this information to determine if, for each file, the information about the files contains text that makes it easy to determine what is being shared. A simple regular expression text search for 'README', 'codebook', 'script', and 'data' (in a number of possible ways that these words can be written) is used to automatically detect what is shared.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Finally, we print a report that communicates to the user - for example, a researcher preparing their manuscript for submission - whether there are suggestions to improve their data repository. We provide feedback about whether any of the four categories could be automatically detected, and if not, provide additional information about what would have made the automated tool recognize the files of interest. The output gives a detailed overview of the information it could not find, alongside a suggestion for how to learn more about best practices in this domain. If researchers use this Papercheck module before submission, they can improve the quality of their data repository in case any information is missing. Papercheck might miss data and code that is shared, but not clearly named. This might be considered a Type 2 error by Papercheck, but our philosophy is that it might still correctly indicate that the data repository can be improved by more clearly naming folders and files, and that this would not only benefit automatic recognition of what is in a repository, but also helps people to find files in a repository.

So far we have used Papercheck to automatically check whether certain types of files exist. But it is also possible to automatically download files, examine their contents, and provide feedback to users. This can be useful to examine datasets (e.g., do files contain IP addresses or other personal information), or code files. Papercheck also contains a function `osf_file_download` to download all files from a project on the OSF while maintaining the folder structure. This should make it easier for reviewers to check all the content shared on the OSF.

= Checking Preregistrations
<checking-preregistrations>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
It is increasingly common for researchers to preregister their studies (#link(<ref-imai2025pre>)[Imai et al., 2025];; #link(<ref-spitzer2023registered>)[Spitzer & Mueller, 2023];). As preregistration is a new practice, it is not surprising that it is not always implemented well. One challenge is that researchers do not always indicate where they deviated from their preregistration when reporting results from preregistered studies (#link(<ref-van2024potential>)[Akker et al., 2024];). Reviewers could check whether researchers adhere to the preregistration, but this requires some effort. Automation can provide a partial solution be making information more easily available, and perhaps even performing automated checks of some parts of the preregistration.

Here we demonstrate how Papercheck can automatically retrieve the content of a preregistration. The preregistration can then be presented alongside the relevant information in the manuscript. This makes it easier for peer reviewers to compare the information.

We focus on AsPredicted preregistrations as their structured format makes it especially easy to retrieve information

= Checking Power Analyses
<checking-power-analyses>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Researchers should provide a sample size justification when they design a study to provide an argument why the study has sufficient informational value with respect to its inferential goal (Lakens, 2022). One approach to a sample size justification is a power analysis. Peers need to be able to evaluate the power analysis to examine with the arguments for the informational value of the study design are valid. This requires that all parameters of the power analysis are reported: The type of power analysis (e.g., a-priori, comprimise, sensitivity), the statistical test that the power analysis is performed for (e.g., an independent t-test, a within-between interaction effect in a 2x2 ANOVA), the effect size metric (e.g., d, r, odds ratio), the effect size (a numerical value, e.g., 0.3), the desired power (e.g., 95%), and the alpha level (commonly 0.05, but it is possible that this value needs to be adjusted for multiple comparisons). Many power analyses do not report all the required information. Moreover, some researchers perform post-hoc power analyses where they compute the power for the observed effect size, given the collected sample size. This flawed practice is performed in an attempt to answer the question whether a non-significant result is a Type 2 error, which post-hoc power can not answer.

The powercheck module aims to assist researchers in fully reporting power analysis, and dissuade them from performing a post-hoc power analysis. It first searches for the word "power\[ed\]", and based on a pre-trained local logistic regression model classifies sentences as a statistical power analysis, or not. Sentences classified as probable power analysis statements are then expanded by including the previous and two subsequent sentences, and sent to Groq, a service for artificial intelligence inference. Groq can run a range of different language models, such as Llama 3.3 70B, an open source language model. Based on a prompt the Groq API returns a JSON structured dataset with either the content of the parameters that should be reported in a power analysis, or missing information if the language model can not retrieve the values for certain parameters.

The information retrieved from the language model is then cleaned (e.g., by removing spaces) and standardized (e.g., Cohen’s d, ds, and Hedges’ g are all recoded as 'd', 0.8, 80%, and .80 are recoded as 0.80, etc.). An additional simple text search for words such as 'post-hoc power' or 'observed power' is performed. Based on this information users are informed about which parameters were not automatically identified, or why observed power analyses should not be informed. Given the complexity of natural language, we should expect Type 1 and type 2 errors. For an example , the sentence "Null of 33% power Note: The observed p -curve includes 29 statistically significant (p \< .05) results, of which 14 are p \< .025. There were 8 additional results entered but excluded from p-curve because they were p \> .05." was incorrectly classified as a sentence describing a power analysis (i.e., a Type 1 error) dus to the presence of a significance level threshold, and numbers of entities included and excluded in the analysis. The following sentence is an example of a Type 2 error: "We attempted to recruit 200 participants to have sufficient power to detect a small to moderate effect size, but in recruiting groups of friends on campus, we ended up with 199 fluent English speakers (105 female)". This statement provides too minimal information about the details of the power analysis to be classified as one.

Type 1 errors are annoying, but this annoyance is hopefully in the long run compensated by the benefits of a tool such as papercheck. Type 2 errors miss an opportunity to inform users of best practices. However, it is possible to still report the result of the power module by stating that no reported power analyses were identified, and informing users that if this is incorrect, they might want to reconsider whether the way they reported the power analysis is clear enough for human readers. A statement such as "We performed an a-priori power analysis for a one-sided dependent t-test with an alpha of 0.05, a desired power of 0.95, an effect size of interest of dz = 0.2, which lead to a required sample size of N = 272" should have a very high probability of being automatically detected and should allow all parameters to be automatically retrieved by the algorithm. Out of all 707 power analysis statements (although these include some double counted due to overlapping sentences) 72 were fully reported. Some missing information might not be considered too problematic, such as the alpha level which is almost exclusively 5% (although some researchers used 0.01 or corrected the alpha level for multiple comparisons) or the test (which can sometimes - although too often not - be retrieved from the result section. The simple warning for observed power based simply on a regex text search would catch the use of 'post-hoc' power in 45 instances across 37 papers. This bad practice is a rare occurrence, but flaw is relatively serious, and hence worth correcting through an automated tool.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Now we ask an LLM to process the power analysis statements.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Now we can take a look at how often which info is missing, if power analyses increase over time, etc. We first merge everything into psych\_science

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We can now summarize this data.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
What would our feedback from the papercheck app look like?

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Percentage of papers with a power analysis per year:

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We should summarize that 1) Not all power analyses are detected, and it is important to increase the accuracy of the workflow. There are false positives and false negatives. At the same time, for the 1838 papers in psych science, we can currently identify and extract information for 445 papers, which contain 707 sentences identified as power analyses. In these 445 we can warn people for missing statistical tests 334 times, missing alpha levels 296 times, missing effect sizes 106 times, missing type of effect sizes 142 times, desired power in 108 papers and the final sample size in XX.

= Text search
<text-search>
= Hyperlinks
<hyperlinks>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Many scientific articles link to additional information that is not included in the article, such as open data, materials, and code, a preregistration, or supplementary information. Although it is useful to have access to this information, it can also be cumbersome to retrieve it. Journals lack a systematic way of providing access to information outside of the article, and the additional information that is provided is often not structured well. Although this is expected as science transitions to more transparency, the fact that supplementary information is often not easily findable or accessible limits its use. Automation can assists researchers by making information more readily available.

One starting point to retrieve information is a hyperlink. We can retrieve hyperlinks from pdf articles in two ways. First, GROBID automatically extracts hyperlinks (i.e., text that can be clicked to open an external website). Second, regular expressions can identify hyperlinks that can not be clicked based on common text patterns in hyperlinks (e.g., the presence of http or www, followed by a string of characters). To capture all hyperlinks it is often necessary to iteratively search the text. Text parsing often struggles with hyperlinks, and it is common to see line breaks in hyperlinks that result in hyperlinks being split across two sentences or having spaces in the hyperlink that need to be removed. Identifying and fixing hyperlinks requires knowledge about the structure of hyperlinks (e.g., links to the Open Science Framework or AsPredicted have either 5 or 6 characters after the domain).

We will provide two illustrations of the usefulness of combining domain expertise with automatically detecting hyperlinks. The first example pertains to the presence of links to anonymized files that are used during the peer review process in published articles. Many journals anonymize authors, which requires linking to anonymized supplementary files. This can be achieved on AsPredicted through a special 'blind' link (e.g., https:\/\/aspredicted.org/blind.php?x=5tq3pg) or a 'view\_only' link to the OSF. Sometimes, these links make it into the published article, even though they should be replaced by the public link after acceptance.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
OSF Links

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We clean up the links we have found, and extract the OSF ID:

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We can now check if the OSF view\_only link goes to a page that is still closed.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
To summarize, in `length(psych_science$doi)` articles published in psychological science we observed `length(text_found$text)` unique view-only links to the OSF. Of these, `sum(text_found$osf_type == "CLOSED")` are not made public. Similarly, we found `length(text_found_as_predicted_blind$text)` unique blind links to AsPredicted, which should have linked to their public counterparts.

= Content of preregistration
<content-of-preregistration>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
First run our code on all OSF links:

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Then we can take the preregistrations, and do something with them, similar to Aspredicted below.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We can again use the llm to get information form non-structured preregs.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Now we should be able to match the sample size to the main paper.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
A task now is to manually compare the llm output and the sample size.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Then we search through all projects:

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Now we can add as predicted sample size info to our psych science dataset.

= List files
<list-files>
= Menial tasks
<menial-tasks>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
There is a range of common mistakes that can easily be automatically checked. We will illustrates the usefulness of Papercheck to perform such menial tasks in X examples.

The first example is to identify the presence of anonimized links in the published version of articles. Researchers often want to make their preregistration or files available to reviewers, but links to online repositories would reveal their identity, which is not in line with the blinding a manuscript for peer review. To solve this problem platforms such as the Open Science Framework (OSF) and AsPredicted have created 'view only' or 'blind' links, which allow peer reviewers to gian access to preregistrations and files, without revealing the name of the researchers. Researchers often forget to change these blinded links to the public links. A simple text search is sufficient to find blinded links in manuscripts, and remind authors (or editors) to update these links when checking proofs.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We find 5 papers which have been published with blind AsPredicted links, and 32 with view only OSF links.

== Check pubpeer comments for references.
<check-pubpeer-comments-for-references.>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We can check pubpeer for comments:

We will just link to the comments, maybe some text summary, but that is it. Optionally, remove statcheck ones.

For example, the DOI https:\/\/doi.org/10.1037/0022-3514.81.4.657 has a pubpeer comment. It is a statcheck automated test comment, so not interesting. We first encode the DOI in URL format, then turn it into the search command:

= Work with references
<work-with-references>
= Discussion
<discussion>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
To prevent widespread misuse of an automated tool such as Papercheck, it is important to realize the limits of the tool. Although it might seem desirable to develop modules with high accuracy, a cost-benefit analysis might indicate that at this phase it is more important to invest time to develop 10 modules that assist researchers to check their papers with a relatively high error rate, then to spend the same time on two modules that can be used meta-scientifically with very low error rates. The flexibility of human language combined with the lack of standardization in how researchers report information in scientific manuscripts are such that reducing error rates from 50% to 20% is much easier than reducing error rates from 20% to 5%. Our current approach is to follow the Pareto principle and focus on the 20% of the ways in which researchers get something wrong, that when when identified, would prevent 80% of the times researchers get something wrong.

= References
#set par(first-line-indent: 0in, hanging-indent: 0.5in)
#block[
#block[
Aczel, B., Palfi, B., Szollosi, A., Kovacs, M., Szaszi, B., Szecsi, P., Zrubka, M., Gronau, Q. F., Bergh, D. van den, & Wagenmakers, E.-J. (2018). Quantifying support for the null hypothesis in psychology: An empirical investigation. #emph[Advances in Methods and Practices in Psychological Science];, #emph[1];(3), 357–366.

] <ref-aczel2018quantifying>
#block[
Akker, O. R. van den, Bakker, M., Assen, M. A. van, Pennington, C. R., Verweij, L., Elsherif, M. M., Claesen, A., Gaillard, S. D., Yeung, S. K., Frankenberger, J.-L., et al. (2024). The potential of preregistration in psychology: Assessing preregistration producibility and preregistration-study consistency. #emph[Psychological Methods];.

] <ref-van2024potential>
#block[
Bendel, R. B., & Afifi, A. A. (1977). Comparison of stopping rules in forward “stepwise” regression. #emph[Journal of the American Statistical Association];, #emph[72];(357), 46–53.

] <ref-bendel1977comparison>
#block[
Dexter, F., & Shafer, S. L. (2017). Narrative review of statistical reporting checklists, mandatory statistical editing, and rectifying common problems in the reporting of scientific articles. #emph[Anesthesia & Analgesia];, #emph[124];(3), 943–947.

] <ref-dexter2017narrative>
#block[
Ferguson, J., Littman, R., Christensen, G., Paluck, E. L., Swanson, N., Wang, Z., Miguel, E., Birke, D., & Pezzuto, J.-H. (2023). Survey of open science practices and attitudes in the social sciences. #emph[Nature Communications];, #emph[14];(1), 5401.

] <ref-ferguson2023survey>
#block[
Goos, C., Bakker, M., Wicherts, J. M., & Nuijten, M. B. (2024). #emph[Assessing reliable and valid measurement as a prerequisite for informative replications in psychology];.

] <ref-goos2024assessing>
#block[
Hosmer Jr, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). #emph[Applied logistic regression];. John Wiley & Sons.

] <ref-hosmer2013applied>
#block[
Imai, T., Toussaert, S., Baillon, A., Dreber, A., Ertaç, S., Johannesson, M., Neyse, L., & Villeval, M. C. (2025). #emph[Pre-registration and pre-analysis plans in experimental economics];. JSTOR.

] <ref-imai2025pre>
#block[
Lakens, D. (2022). Sample size justification. #emph[Collabra: Psychology];, #emph[8];(1), 33267. #link("https://doi.org/10.1525/collabra.33267")

] <ref-lakens_justification_2022>
#block[
Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for psychological research: A tutorial. #emph[Advances in Methods and Practices in Psychological Science];, #emph[1];(2), 259–269. #link("https://doi.org/10.1177/2515245918770963")

] <ref-lakens_equivalence_2018>
#block[
Sharpe, D. (2013). Why the resistance to statistical innovations? Bridging the communication gap. #emph[Psychological Methods];, #emph[18];(4), 572.

] <ref-sharpe2013resistance>
#block[
Spitzer, L., & Mueller, S. (2023). Registered report: Survey on attitudes and experiences regarding preregistration in psychological research. #emph[Plos One];, #emph[18];(3), e0281086.

] <ref-spitzer2023registered>
#block[
Tendeiro, J. N., Kiers, H. A., Hoekstra, R., Wong, T. K., & Morey, R. D. (2024). Diagnosing the misuse of the bayes factor in applied research. #emph[Advances in Methods and Practices in Psychological Science];, #emph[7];(1), 25152459231213371.

] <ref-tendeiro2024diagnosing>
#block[
Thibault, R. T., Zavalis, E. A., Malički, M., & Pedder, H. (2024). An evaluation of reproducibility and errors in published sample size calculations performed using g\* power. #emph[medRxiv];, 2024–2007.

] <ref-thibault2024evaluation>
] <refs>
#set par(first-line-indent: 0.5in, hanging-indent: 0in)


 
  
#set bibliography(style: "\_extensions/wjschne/apaquarto/apa.csl") 


