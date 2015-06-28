module Handler.Home where

import Import
import Text.Blaze (Markup)

encode :: Text -> Markup
encode s = preEscapedToMarkup $ concatMap ((++";") . ("&#"++) . show . fromEnum) s 

email :: Markup
email = encode ("medlock@unm.edu" :: Text)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    let route = HomeR
    defaultLayout $ do
        setTitle "Jordan E Medlock - Home"
        $(widgetFile "menu")
        $(widgetFile "homepage")

getBioR :: Handler Html
getBioR = do
  defaultLayout $ do
    [whamlet|
    <div .panel .panel-default>
      <div .panel-heading>
        <h2 .panel-title>
          About Me 
      <div .panel-body .bio>
        <img .img-responsive .img-rounded .me .pull-right src="/static/img/me_and_honey.png" alt="Me and my fiance standing over Glenn Canyon Dam">
        <p>
          I am a student at the University of New Mexico working on my 
          Computer Science degree. I love to create new things and I love 
          to teach.
        <p>
          I am currently attending the University of New Mexico, working to obtain my bachelor's degree in Computer Science. I have 6 years experience in Computer Science, including 4 years participating in the New Mexico Supercomputing Challenge. In my last year participating in the Challenge, I won first place for my work on a program which identifies Alzheimer's plaque in transgenic mouse brains. 
    <div .panel .panel-default>
      <div .panel-heading>
        <h2 .panel-title>
          Contact Me 
      <div .panel-body .bio>
        <p>
          <a href="mailto:#{email}">
            Email
          <br>
          <a href="https://www.linkedin.com/in/jordanemedlock">
            LinkedIn
    <div .panel .panel-default>
      <div .panel-heading>
        <h2 .panel-title>
          Projects 
      <div .panel-body .bio>
        <ol>
          <li>
            <a href="http://uaft.org">
              Umali Autism Foundation Tanzania
          <li>
            <a href="http://jordanemedlock.com:8888">
              Digital Aristotle

          


    |]

            --   <li>
            -- <a href="http://jordanemedlock.com:8000">
            --   An Informatics Platform for Natural Product Drug Discovery
