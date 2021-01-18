
function(){
  tabPanel(h1(id = "panel_about", "About"),
           HTML('
      <div style="display:inline;float:right;margin:0px 0px 5px 20px">
      </div>

      <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify">
          This Shiny application allows to test your botanical knowledge of the German flora.
          In the first tab, you will retrieve the information about each species.
          The second tab is a quizz.
          
          <br>
          
          <a href="https://www.uni-goettingen.de/en/128741.html" target=_blank>Biodiversity, Macroecology and Biogeography</a>. 
        </p>
      </div>   

      <br>

      <hr width="1000", align="left" style="height:0.5px;border:none;color:#A0A5A8;background-color:#A0A5A8;" />
  
		  		  <span style="color:#64645F;font-weight: bold;">Authors</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify">
		        <a href="https://www.uni-goettingen.de/en/157014.html" target=_blank>Patrick Weigelt</a> & Pierre Denelle
		     </p>
		  </div>  

      <span style="color:#64645F;font-weight:bold;">Sources</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify;">
		     
		     <a href="https://www.floraweb.de/" target=_blank>FloraWeb</a>
		     
		     </p>
		  </div>  

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
