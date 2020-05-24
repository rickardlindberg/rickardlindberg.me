JUMBOTRON1='<div class="jumbotron"><p>This article is about how I created the RLMeta poster. Do you want to get the poster yourself?<\/p><p><a class="btn btn-primary btn-lg" href="\/pages\/rlmeta-poster\/index.html" role="button">Check this page out!<\/a><\/p><\/div>'

JUMBOTRON2='<div class="jumbotron"><p>Do you want to have this poster in your hands as well?<\/p><p><a class="btn btn-primary btn-lg" href="\/pages\/rlmeta-poster\/index.html" role="button">Check this page out!<\/a><\/p><\/div>'

gen() {
    html=$(rliterate index.rliterate --html)
    echo "---"
    echo "title: '$(echo "$html" | head -n1 | cut -c52- | cut -d'<' -f1)'"
    echo "date: 2020-05-24"
    echo "tags: rlmeta"
    echo "---"
    echo "$html" | tail -n+2 | sed "s/^<p>JUMBOTRON PLACEHOLDER 1<\\/p>$/$JUMBOTRON1/" | sed "s/^<p>JUMBOTRON PLACEHOLDER 2<\\/p>$/$JUMBOTRON2/"
}

gen > index.html
