package net.ionoff.notify.esms;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringReader;
import java.net.HttpURLConnection;
import java.net.URL;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import net.ionoff.notify.CommonUtil;
import net.ionoff.notify.ISmsService;
import net.ionoff.notify.NotifyConfig;

public class SmsServiceImpl implements ISmsService {
	
	private static final Logger LOGGER = Logger.getLogger(SmsServiceImpl.class.getName());
	
	@Override
	public String sendSms(String[] subscribers, String message) throws IOException {
		LOGGER.info("Sending SMS to: " + CommonUtil.toString(subscribers));
		LOGGER.info("Sending SMS message: " + message);
		
		String url = NotifyConfig.getInstance().SMS_GATEWAY_URL;
		URL obj = new URL(url);
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();
		
		SmsRequestBody requestBody = new SmsRequestBody(
				NotifyConfig.getInstance().SMS_GATEWAY_API_KEY, 
				NotifyConfig.getInstance().SMS_GATEWAY_SECRET_KEY, subscribers, message);
		String postData = requestBody.toXmlFormat();
		
		con.setDoOutput(true);
		con.setRequestMethod("POST");
		con.setFixedLengthStreamingMode(postData.getBytes().length);
		con.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
		// send the POST out
		PrintWriter out = new PrintWriter(con.getOutputStream());
		out.print(postData);
		out.close();
		
		int responseCode = con.getResponseCode();
		LOGGER.info("Response code from SMS gateway: " + responseCode);
		BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
		String inputLine;
		StringBuffer response = new StringBuffer();
		while ((inputLine = in.readLine()) != null) {
			response.append(inputLine);
		}
		in.close();
		String resp = response.toString();
		LOGGER.info("Response message from SMS gateway: " + resp);
		if (responseCode == 200) {
			SmsResultModel result;
			try {
				result = toSmsResultModel(resp);
			} catch (Exception e) {
				LOGGER.error("Failed to parse SmsResultModel. " + e.getMessage());
				return "Failed to parse SmsResultModel. " + e.getMessage();
			}
			return toMessage(result);
		}
		return resp;
	}

	private String toMessage(SmsResultModel result) {
		if ("100".equals(result.getCodeResult())) {
			return "100: Success";
		}
		if ("99".equals(result.getCodeResult())) {
			return result.getCodeResult() + ": Unknown error";
		}
		if ("101".equals(result.getCodeResult())) {
			return result.getCodeResult() + ": Invalid API key or Secret key";
		}
		if ("102".equals(result.getCodeResult())) {
			return result.getCodeResult() + ": The account is blocked";
		}
		if ("103".equals(result.getCodeResult())) {
			return result.getCodeResult() + ": Account balance is not enough";
		}
		if ("104".equals(result.getCodeResult())) {
			return result.getCodeResult() + ": Brandname code  is not valid";
		}
		if ("119".equals(result.getCodeResult())) {
			return result.getCodeResult() + ": Unknown serial number";
		}
		if ("120".equals(result.getCodeResult())) {
			return result.getCodeResult() + ": Unknown card number";
		}
		if ("121".equals(result.getCodeResult())) {
			return result.getCodeResult() + ": Unknown SMS service provider";
		}
		if ("122".equals(result.getCodeResult())) {
			return result.getCodeResult() + ": Failed to recharge";
		}
		return result.getCodeResult() + ": No description";
	}

	private SmsResultModel toSmsResultModel(String resp) throws ParserConfigurationException, SAXException, IOException {
		SmsResultModel resultModel = new SmsResultModel();
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = dbf.newDocumentBuilder();
	    InputSource is = new InputSource();
	    is.setCharacterStream(new StringReader(resp));
	    Document doc = db.parse(is);
		doc.getDocumentElement().normalize();
		final Element docElement = doc.getDocumentElement();
		for (int i = 0; i < docElement.getChildNodes().getLength(); i++) {
			Node node = docElement.getChildNodes().item(i);
			if (node.getNodeName().equals("CodeResult")) {
				resultModel.setCodeResult(node.getTextContent());
			}
			else if (node.getNodeName().equals("SMSID")) {
				resultModel.setSmsId(node.getTextContent());
			}
			else if (node.getNodeName().equals("CountRegenerate")) {
				resultModel.setCountRegenerate(node.getTextContent());
			}
			else if (node.getNodeName().equals("IsSandbox")) {
				resultModel.setIsSandbox(node.getTextContent());
			}
		}
		return resultModel;
	}
}
