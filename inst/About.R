
function(){
  tabPanel(h1(id = "panel_about", "About"),
           HTML('
      <div style="display:inline;float:right;margin:0px 0px 5px 20px">
      </div>
		  <span style="color:#64645F;font-weight: bold;">General description</span>
		  <br>
		  <br>
      <div style="max-width:800px; word-wrap:break-word;">
        <p style="font-size:100%;text-align:justify">
          The BotanizeR Shiny application allows to test and improve your botanical 
          knowledge for a defined set of species based on a defined set of images 
          and ecological and morphological descriptions. 
          <br>
          <br>
          In the Species tab, you can retrieve all deposited information for each species.
          <br>
          <br>
          In the Quiz tab you can practice your species identification skills. 
          The app will show you random species and you are asked to provide the 
          species name (case insensitive). You may browse through a selection of 
          pictures and enable additional hints based on species morphological and 
          ecological descriptions. The app gives feedback whether your attempt was 
          correct or not and if not, how different the actual name is and whether 
          the genus was correct or not. The quiz keeps track of the species shown 
          and answered correctly and uses this information to show species that 
          have been answered correctly less frequently and species which have not 
          been answered correctly more frequently. You can download the species list 
          after practicing and upload it again to make use of the saved numbers of 
          successes and failures when practicing again.
          <br>
          <br>
          In the Setup tab you can choose which species to practice, either by 
          choosing from a predefined set of species lists or by uploading a customized 
          species list. Here, you can also define which hints and images to make 
          available in the Species and Quiz tabs based on all the resources available 
          in the given instance of BotanizeR. 
          <br>
          <br>
      </p>
      <span style="color:#64645F;font-weight: bold;">This Example</span>
		  <br>
		  <br>
      <div style="max-width:800px; word-wrap:break-word;">
        <p style="font-size:100%;text-align:justify">
          This particular instance of the BotanizeR Shiny app exemplifies a few 
          particular use cases. Here, you can practice species from the Floras of 
          Germany, Britain and Ireland based on information retrieved live from 
          the websites of 
          <a href="https://www.floraweb.de/" target=_blank>FloraWeb</a> 
          and the 
          <a href="https://www.brc.ac.uk/plantatlas/" target=_blank>UK & Ireland Plant Atlas</a>. 
          You can choose which of the Floras to practice species from, which species 
          subset to use and which information from each of the websites to show. 
          In addition, you can choose to include images of woody plants of Germany 
          in winter state as an example of images provided by the host of the app 
          or the French common name as an example of a hint provided by the host 
          of the app. The app starts with a list of 314 common or characteristic 
          species from Central Germany, including images from all three resources 
          mentioned above. In the settings tab you can switch to the entire Flora 
          of Britain and Ireland including only images and hints from the 
          <a href="https://www.brc.ac.uk/plantatlas/" target=_blank>UK & Ireland Plant Atlas</a> 
          or to a list of 128 woody species from Germany with images available in 
          winter state. If you disable other image sources you can practice these 
          species only based on their characteristics as found during the winter 
          months.
          <br>
          <br>
      </p>
      <span style="color:#64645F;font-weight: bold;">General applicability</span>
		  <br>
		  <br>
        <p style="font-size:100%;text-align:justify">
          BotanizeR is highly flexible and easy to set up, allowing lecturers to 
          define themselves the species list, picture base and species characteristics 
          to show to their students. Technically, BotanizeR is an R-package which 
          can be installed from 
          <a href="https://github.com/patrickweigelt/BotanizeR" target=_blank>GitHub</a>. 
          The quiz can be played within R or better RStudio or alternatively it 
          can be made available via the package&rsquo;s shiny app. The shiny app can be 
          launched locally after installing the package or on a shiny server to 
          make it accessible to a broader audience. When using it locally students 
          could be provided with a species list with descriptions and an image folder 
          for download which can then be loaded from within the shiny app and used 
          for practicing. When launching it on a shiny server, BotanizeR can be 
          accessed online via an URL and species lists, descriptions and pictures 
          can be made available right away within the app like exemplified here.
          <br>
          <br>
        </p>
      </div>   
      <hr width="800", align="left" style="height:0.5px;border:none;color:#A0A5A8;background-color:#A0A5A8;" />
      <br>
		  <span style="color:#64645F;font-weight: bold;">Authors</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify">
		        The BotainzeR R-package and Shiny app have been developed at <a href="https://www.uni-goettingen.de/en/128741.html" target=_blank>Biodiversity, Macroecology and Biogeography</a> 
		         by <a href="https://www.uni-goettingen.de/en/157014.html" target=_blank>Patrick Weigelt</a> & Pierre Denelle.
		     </p>
		  </div>  
      <br>
      <span style="color:#64645F;font-weight:bold;">Sources</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify;">
		     
		     This instance of BotanizeR retrieves images and information from 
		     <a href="https://www.floraweb.de/" target=_blank>FloraWeb</a> and the 
		     <a href="https://www.brc.ac.uk/plantatlas/" target=_blank>UK & Ireland Plant Atlas</a>. 
		     Please visit these websites for more informationan about sources and image authors. 
		      <br>
          <br>
         In addition, images of plants in winter state have been provided by 
         F. Brambach, C. D&ouml;nges, J. Kuper and H. Reichelt from 
         <a href="https://www.uni-goettingen.de/en/128741.html" target=_blank>Biodiversity, Macroecology and Biogeography</a> 
         which are licensed under Creative Commons Attribution-ShareAlike 4.0 International License 
         (<a href="http://creativecommons.org/licenses/by-sa/4.0" target=_blank>CC BY-SA 4.0</a>)

		     </p>
		  </div>  
      <br>
		  <span style="color:#64645F;font-weight:bold;">License</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify;">
		        Coded under License GPLv3
		     </p>
		  </div> 
		'),
           value = "about"
  )
}
