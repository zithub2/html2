w = x=>document.write(x);
words = s=>Array.from(s.matchAll(/\S+/g));
withTag = (tag,inner)=>toOpenTag(tag)+inner+toCloseTag(tag);
toOpenTag = tag=>"<"+tag+">";
toCloseTag = tag=>"</"+tag+">";

pc=(p,c,inners)=>withTag(p,inners.map(s=>withTag(c,s)).join(''));
tr=(c,inners)=>pc("tr",c,inners);

html2={
  th: s=>w(tr("th",words(s))),
  td: s=>w(tr("td",words(s))),
};
