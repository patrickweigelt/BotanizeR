
function(){
  tabPanel(h1(id = "panel_about", "About"),
           HTML(paste0(
             '
      <div style="display:inline;float:right;margin:0px 0px 5px 20px">
      </div>
		  <span style="color:#64645F;font-weight: bold;">General description</span>
		  <br>
		  <br>
      <div style="max-width:800px; word-wrap:break-word;">
        <p style="font-size:100%;text-align:justify">
          The BotanizeR Shiny application allows to test and improve your 
          botanical knowledge for a defined set of species based on a defined 
          set of images and ecological and morphological descriptions. 
          <br>
          <br>
          In the Species tab, you can retrieve all deposited information for 
          each species.
          <br>
          <br>
          In the Quiz tab you can practice your species identification skills. 
          The app will show you random species and you are asked to provide the 
          species name (case insensitive). You may browse through a selection 
          of pictures and enable additional hints based on species 
          morphological and ecological descriptions. The app gives feedback on 
          whether your attempt was correct or not and if not, how different the 
          actual name is and whether the genus was correct or not. The quiz 
          keeps track of the species shown and answered correctly and uses this 
          information to show species that ave been answered correctly less 
          frequently and species which have not been answered correctly more 
          frequently (if enabled). You can download the species list after 
          practicing and upload it again to make use of the saved numbers of 
          successes and failures when practicing again.
          <br>',
             ifelse(setup, '<br>
          In the Setup tab you can choose which species to practice, either by 
          choosing from a predefined set of species lists or by uploading a 
          customized species list.', 
                    ''),
             ifelse(setup & gbif, ' You can subset the species list by those 
             species found in <a target="_blank" href= "https://www.gbif.org/">
                    GBIF</a> for a defined radius around a desired location.', 
                    ''),
             ifelse(setup, ' You can also define which hints and images to make 
          available in the Species overview and Quiz tabs based on all the 
          resources available in this instance of BotanizeR and hints you added 
          to the species list. 
          <br>', ''),
             '<br>
      </p>
      <span style="color:#64645F;font-weight: bold;">This Example</span>
		  <br>
		  <br>
      <div style="max-width:800px; word-wrap:break-word;">
        <p style="font-size:100%;text-align:justify">',
             instance_description,
        '<br>
         <br>
      </p>
      <span style="color:#64645F;font-weight: bold;">
      General applicability</span>
		  <br>
		  <br>
        <p style="font-size:100%;text-align:justify">
          BotanizeR is highly flexible and easy to set up, allowing lecturers 
          to define themselves the species list, picture base and species 
          characteristics to show to their students. Technically, BotanizeR is 
          an R-package which can be installed from 
          <a href="https://github.com/patrickweigelt/BotanizeR" target=_blank>
          GitHub</a>. Tutorials on how to setup BotanizeR are vailable 
          <a href="https://patrickweigelt.github.io/BotanizeR/index.html" 
          target=_blank>here</a>.
          The quiz can be played within R or better RStudio or alternatively it 
          can be made available via the package&rsquo;s Shiny app. The Shiny 
          app can be launched locally after installing the package or on a 
          Shiny server to make it accessible to a broader audience. When using 
          it locally students could be provided with a species list with 
          descriptions and an image folder for download which can then be 
          loaded from within the Shiny app and used for practicing. When 
          launching it on a Shiny server, BotanizeR can be accessed online via 
          an URL and species lists, descriptions and pictures can be made 
          available right away within the app like exemplified here.
          <br>
          <br>
        </p>
      </div>   
      <hr width="800", align="left" style="height:0.5px;border:none;
      color:#A0A5A8;background-color:#A0A5A8;" />
      <br>
      <span style="color:#64645F;font-weight:bold;">Sources</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify;">',
         instance_credits,
		     '</p>
		  </div>  
      <br>
		  <span style="color:#64645F;font-weight: bold;">Authors</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify">
		        The BotanizeR R-package and Shiny app have been developed at 
		        <a href="https://www.uni-goettingen.de/en/128741.html" 
		        target=_blank>Biodiversity, Macroecology and Biogeography</a> 
		         by <a href="https://www.uni-goettingen.de/en/157014.html" 
		         target=_blank>Patrick Weigelt</a> & Pierre Denelle.
		     </p>
		  </div>  
      <br>
		  <span style="color:#64645F;font-weight: bold;">Citation</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify">
		     Weigelt, P., Denelle, P., Brambach, F. & Kreft, H. (2021) A flexible 
         R package with Shiny app to practice plant identification for 
         online teaching and beyond. <i>PLANTS, PEOPLE, PLANET</i>, 
         <a href="https://doi.org/10.1002/ppp3.10226" target=_blank>
         https://doi.org/10.1002/ppp3.10226</a>.
		     </p>
		  </div>  
		  <br>
		  <span style="color:#64645F;font-weight:bold;">License</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify;">
		        Coded under License GPLv3
		     </p>
		  </div> 
		')),
           value = "about"
  )
}
