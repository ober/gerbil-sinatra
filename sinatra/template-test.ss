(import :std/test
        ./template)

(export template-test)

(def template-test
  (test-suite "template rendering"

    (test-case "render-sxml produces HTML"
      (let ((html (render-sxml '(h1 "Hello"))))
        (check html ? string?)
        (check html => "<h1>Hello</h1>")))

    (test-case "render-sxml with nested elements"
      (let ((html (render-sxml '(div (h1 "Title") (p "Content")))))
        (check html ? string?)
        (check html => "<div><h1>Title</h1><p>Content</p></div>")))

    (test-case "render-sxml with attributes"
      (let ((html (render-sxml '(a (@ (href "/home")) "Home"))))
        (check html ? string?)
        (check html => "<a href=\"/home\">Home</a>")))

    (test-case "render-string with variables"
      (let ((result (render-string "Hello, {{name}}!"
                      (hash ("name" "World")))))
        (check result => "Hello, World!")))

    (test-case "render-string with multiple variables"
      (let ((result (render-string "{{greeting}}, {{name}}!"
                      (hash ("greeting" "Hi") ("name" "Alice")))))
        (check result => "Hi, Alice!")))

    (test-case "render-string with missing variable"
      (let ((result (render-string "Hello, {{name}}!"
                      (hash))))
        (check result => "Hello, !")))

    (test-case "render-string with no variables"
      (let ((result (render-string "Static text" (hash))))
        (check result => "Static text")))

    (test-case "render-string with braces in text"
      (let ((result (render-string "x = {y}" (hash))))
        (check result => "x = {y}")))
  ))
