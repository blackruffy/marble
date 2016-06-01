#include "TObject.h"

void stat() {

  const int nh = 50;
  const int ipix = 10;
  const int epix = 100;
  const int nbin = epix - ipix - 1;

  //TFile* file = new TFile("stat.root", "RECREATE");

  ifstream ifs("stat.txt");
  int i = 0;
  int x;
  float n;
  
  TH1F *h[nh];
  for( int i=0; i<nh; i++ ) {
    h[i] = new TH1F(TString::Format("h%d", i), "h", 150, 0, 150);
  }

  float g[nh][nbin];

  while( !ifs.eof() ) {
    ifs >> i >> x >> n;
    h[i-1]->SetBinContent( x + 1, n );
    if( x > ipix && x < epix ) {
      float pn = h[i-1]->GetBinContent( x );
      g[i-1][x-ipix-1] = abs( n - pn )/pn*100;
    }
  }

  TH1F *hcon = new TH1F("hcon", "hcon", nh, 0, 50000);
  for( int i=0; i<nh; i++ ) {

    float mean = 0;
    for( int x=0; x<nbin;  x++ ) {
      mean += g[i][x]/nbin;
    }
    
    float sd = 0;
    for( int x=0; x<nbin; x++ ) {
      float d = mean - g[i][x];
      float d2 = d*d;
      sd += d2/(nbin-1);
    }
    float err = sqrt(sd);
    cout << i << ", " << mean << ", " << err << endl;
    hcon->SetBinContent( i+1, mean );
    hcon->SetBinError( i+1, err );
    
    //if( i==1 ) h[i]->Draw();
    //gPad->SetGrid();

    //TF1 *f = new TF1("f", "[0] + [1]*x + [2]*x**2 + [3]*x**3 + [4]*x**4 +[5]*x**5", 10, 100);
    //
    //h[i]->Fit(f, "", "", 10, 100);
    //cout << i << " " << f->GetChisquare() << endl;
    //h[i]->Write(0, h[i]->kOverwrite);
  }

  //hcon->Write(0, hcon->kOverwrite);

  //file->Close();

  TCanvas *c;
  
  c = new TCanvas();
  gPad->SetGrid();
  h[0]->SetStats(0);
  h[0]->SetTitle("1000 photons/pixel");
  h[0]->SetYTitle("Number of photons that reached the light");
  h[0]->SetXTitle("Position");
  h[0]->Draw();
  c->SaveAs("1000nphoto.png");

  c = new TCanvas();
  gPad->SetGrid();
  h[10]->SetTitle("10000 photons/pixel");
  h[10]->SetStats(0);
  h[10]->SetYTitle("Number of photons that reached the light");
  h[10]->SetXTitle("Position");
  h[10]->Draw();
  c->SaveAs("10000nphoto.png");

  c = new TCanvas();
  gPad->SetGrid();
  h[30]->SetStats(0);
  h[30]->SetTitle("30000 photons/pixel");
  h[30]->SetYTitle("Number of photons that reached the light");
  h[30]->SetXTitle("Position");
  h[30]->Draw();
  c->SaveAs("30000nphoto.png");

  c = new TCanvas();
  gPad->SetGrid();
  h[49]->SetStats(0);
  h[49]->SetTitle("50000 photons/pixel");
  h[49]->SetYTitle("Number of photons that reached the light");
  h[49]->SetXTitle("Position");
  h[49]->Draw();
  c->SaveAs("50000nphoto.png");
  
  c = new TCanvas();
  gPad->SetGrid();
  hcon->SetStats(0);
  hcon->SetTitle("The convergence");
  hcon->SetXTitle("Number of photons per pixel");
  hcon->SetYTitle("Fluctuation");
  hcon->Draw();
  c->SaveAs("convergence.png");
  
}
