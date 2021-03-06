--------------------------------------------------------------------------------
module Hexyll.Web.Html.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.Char        (toUpper)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit ((@=?))


--------------------------------------------------------------------------------
import           Hexyll.Web.Html
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hexyll.Web.Html.Tests" $ concat
    [ fromAssertions "demoteHeaders"
        [ "<h2>A h1 title</h2>" @=?
            demoteHeaders "<h1>A h1 title</h1>"
        ]

    , fromAssertions "withUrls"
        [ "<a href=\"FOO\">bar</a>" @=?
            withUrls (map toUpper) "<a href=\"foo\">bar</a>"
        , "<img src=\"OH BAR\" />" @=?
            withUrls (map toUpper) "<img src=\"oh bar\" />"

        -- Test escaping
        , "<script>\"sup\"</script>" @=?
            withUrls id "<script>\"sup\"</script>"
        , "<code>&lt;stdio&gt;</code>" @=?
            withUrls id "<code>&lt;stdio&gt;</code>"
        , "<style>body > p { line-height: 1.3 }</style>" @=?
            withUrls id "<style>body > p { line-height: 1.3 }</style>"

        -- Test minimizing elements
        , "<meta bar=\"foo\" />" @=?
            withUrls id "<meta bar=\"foo\" />"
        ]

    , fromAssertions "toUrl"
        [ "/foo/bar.html"                     @=? toUrl "foo/bar.html"
        , "/"                                 @=? toUrl "/"
        , "/funny-pics.html"                  @=? toUrl "/funny-pics.html"
        , "/funny%20pics.html"                @=? toUrl "funny pics.html"
        -- Test various reserved characters (RFC 3986, section 2.2)
        , "/%21%2A%27%28%29%3B%3A%40%26.html" @=? toUrl "/!*'();:@&.html"
        , "/%3D%2B%24%2C/%3F%23%5B%5D.html"   @=? toUrl "=+$,/?#[].html"
        -- Test various characters that are nor reserved, nor unreserved.
        , "/%E3%81%82%F0%9D%90%87%E2%88%80"   @=? toUrl "\12354\119815\8704"
        ]

    , fromAssertions "toSiteRoot"
        [ ".."    @=? toSiteRoot "/foo/bar.html"
        , "."     @=? toSiteRoot "index.html"
        , "."     @=? toSiteRoot "/index.html"
        , "../.." @=? toSiteRoot "foo/bar/qux"
        , ".."    @=? toSiteRoot "./foo/bar.html"
        , ".."    @=? toSiteRoot "/foo/./bar.html"
        ]

    , fromAssertions "isExternal"
        [ True  @=? isExternal "http://reddit.com"
        , True  @=? isExternal "https://mail.google.com"
        , True  @=? isExternal "//ajax.googleapis.com"
        , False @=? isExternal "../header.png"
        , False @=? isExternal "/foo/index.html"
        ]

    , fromAssertions "stripTags"
        [ "foo"     @=? stripTags "<p>foo</p>"
        , "foo bar" @=? stripTags "<p>foo</p> bar"
        , "foo"     @=? stripTags "<p>foo</p"
        ]

    , fromAssertions "escapeHtml"
        [ "Me &amp; Dean" @=? escapeHtml "Me & Dean"
        , "&lt;img&gt;"   @=? escapeHtml "<img>"
        ]
    ]
