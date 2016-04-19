package fr.diaspec.webcam.taxoimpl;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

import org.json.JSONObject;

import android.app.Activity;
import android.content.Intent;
import android.util.Log;
import fr.diaspec.webcam.common.CommonRuncode;
import fr.diaspec.webcam.generated.AbstractIP;


public class IP extends AbstractIP {

	private String ipResult;
	private Activity a;
	
	@Override
	public void init(CommonRuncode a) {
		this.a = a;
		if(ipResult == null)
			startFetchingInBackground();
	}
	
	private void startFetchingInBackground() {
		if(this.a == null) {
			Log.i("ip","going to give a runtime exception");
			throw new RuntimeException("Polling value before initialisation!");
		}
		
		Log.i("gc","trying to get ip.");
		
		// here we just demonstrate an Internet connection.
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
					Log.i("gc","json retrieved: " + result);
					JSONObject jObject = new JSONObject(result);
					final String aJsonString = jObject.getString("origin");

					ipResult = aJsonString;
					IP.this.notify(ipResult);

				} catch (Exception e) {

					e.printStackTrace();

					Log.i("gc","Oops, something went wrong connecting to the internet.");
					ipResult = "connection error";
				}

			}
		}).start();

	}

	@Override
	protected String getIPValue() {
		if(ipResult != null)
			return ipResult;
		else
			return "error with IP, perhaps IP hasn't had time to fetch data.";
	}

	@Override
	public void pingBack(int requestCode, Intent data) {
		// this component does not expect intents broadcast
		// from Android, so never mind -- return immediately.
		
	}

}
