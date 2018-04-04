package net.ionoff.things.e4;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import net.ionoff.things.ConfigTool;
import net.ionoff.things.TcpServer;

public class E4ConfigPanel extends JPanel {
	
	private static final long serialVersionUID = 1L;
	
	private static final String SWITCH = "Switch";
	private static final String BUTTON = "Button";
	
	private ConfigTool tool;
	private E4Config conf;
	
	private JTextField newWifiIdTextField;
	private JTextField newWifiPwTextField;
	private JTextField newBrokerTextField;
	private JTextField newSnTextField;
	private List<JComboBox<String>> newInputTypeComboBoxes;
	private JComboBox<String> newInput1TypeComboBox;
	private JComboBox<String> newInput2TypeComboBox;
	private JComboBox<String> newInput3TypeComboBox;
	private JComboBox<String> newInput4TypeComboBox;

	private JButton loadBtn;
	private JButton submitBtn;

	public E4ConfigPanel(ConfigTool p8Tool) {
		this.tool = p8Tool;
		setLayout(null);
		
		//////////////// --------------------------------------------------------------------
		JPanel newInputTypePanel = new JPanel();
		newInputTypePanel.setBounds(0, 10, 535, 35);

		newInputTypePanel.setLayout(null);
		JLabel newInputTypePanelLbl = new JLabel("Input");
		newInputTypePanelLbl.setBounds(10, 5, 65, 25);
		newInputTypePanel.add(newInputTypePanelLbl);
		
		newInputTypeComboBoxes = new ArrayList<JComboBox<String>>();
				
		newInput1TypeComboBox = new JComboBox<>();
		newInput1TypeComboBox.addItem(SWITCH);
		newInput1TypeComboBox.addItem(BUTTON);
		newInput1TypeComboBox.setBounds(65, 5, 105, 28);
		newInputTypeComboBoxes.add(newInput1TypeComboBox);
		newInputTypePanel.add(newInput1TypeComboBox);
			
		newInput2TypeComboBox = new JComboBox<>();
		newInput2TypeComboBox.addItem(SWITCH);
		newInput2TypeComboBox.addItem(BUTTON);
		newInput2TypeComboBox.setBounds(180, 5, 105, 28);
		newInputTypeComboBoxes.add(newInput2TypeComboBox);
		newInputTypePanel.add(newInput2TypeComboBox);
		
		JLabel newInput3TypePanelLbl = new JLabel("Input 3");
		newInput3TypePanelLbl.setHorizontalAlignment(SwingConstants.CENTER);
		newInput3TypePanelLbl.setBounds(207, 5, 10, 25);
		newInputTypePanel.add(newInput3TypePanelLbl);
		
		newInput3TypeComboBox = new JComboBox<>();
		newInput3TypeComboBox.addItem(SWITCH);
		newInput3TypeComboBox.addItem(BUTTON);
		newInput3TypeComboBox.setBounds(300, 5, 105, 28);
		newInputTypeComboBoxes.add(newInput3TypeComboBox);
		newInputTypePanel.add(newInput3TypeComboBox);
				
		newInput4TypeComboBox = new JComboBox<>();
		newInput4TypeComboBox.addItem(SWITCH);
		newInput4TypeComboBox.addItem(BUTTON);
		newInput4TypeComboBox.setBounds(415, 5, 105, 28);
		newInputTypeComboBoxes.add(newInput4TypeComboBox);
		newInputTypePanel.add(newInput4TypeComboBox);

		add(newInputTypePanel);
		

		////////////////--------------------------------------------------------------------
		JPanel newWifiIdPwPanel = new JPanel();
		newWifiIdPwPanel.setBounds(0, 50, 535, 35);
		newWifiIdPwPanel.setLayout(null);
		
		JLabel newWifiIdLbl = new JLabel("Wifi ID");
		newWifiIdLbl.setBounds(10, 5, 65, 25);
		newWifiIdPwPanel.add(newWifiIdLbl);
		newWifiIdTextField = new JTextField(20);
		newWifiIdTextField.setBounds(65, 5, 220, 25);
		newWifiIdPwPanel.add(newWifiIdTextField);
		
		JLabel newWifiPwLbl = new JLabel("Wifi Pw");
		newWifiPwLbl.setBounds(300, 5, 65, 25);
		newWifiIdPwPanel.add(newWifiPwLbl);
		newWifiPwTextField = new JTextField(20);
		newWifiPwTextField.setBounds(365, 5, 155, 25);
		newWifiIdPwPanel.add(newWifiPwTextField);
		
		add(newWifiIdPwPanel);
		
		//////////////// --------------------------------------------------------------------
		JPanel newServerIpMacPanel = new JPanel();
		newServerIpMacPanel.setBounds(0, 85, 535, 45);
		newServerIpMacPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
		newServerIpMacPanel.setLayout(null);

		JLabel newServerIpLbl = new JLabel("Server");
		newServerIpLbl.setBounds(10, 5, 65, 25);
		newServerIpMacPanel.add(newServerIpLbl);
		newBrokerTextField = new JTextField(20);
		if (ConfigTool.PUBLIC_MODE) {
			newBrokerTextField.setBounds(65, 5, 455, 25);
		}
		else {
			newBrokerTextField.setBounds(65, 5, 220, 25);
		}
		newServerIpMacPanel.add(newBrokerTextField);

		JLabel newMacLbl = new JLabel("MAC");
		newMacLbl.setBounds(300, 5, 65, 25);
		if (ConfigTool.PUBLIC_MODE) {
			newMacLbl.setVisible(false);
		}
		newServerIpMacPanel.add(newMacLbl);
		newSnTextField = new JTextField(20);
		newSnTextField.setBounds(365, 5, 155, 25);
		if (ConfigTool.PUBLIC_MODE) {
			newSnTextField.setVisible(false);
		}
		newServerIpMacPanel.add(newSnTextField);

		add(newServerIpMacPanel);

		//////////////// --------------------------------------------------------------------
		JPanel buttonsPanel = new JPanel();
		buttonsPanel.setBounds(0, 130, 535, 40);
		buttonsPanel.setLayout(null);
		
		loadBtn = new JButton("Load");
		loadBtn.setBounds(295, 10, 90, 28);
		buttonsPanel.add(loadBtn);
		
		submitBtn = new JButton("Submit");
		submitBtn.setBounds(395, 10, 125, 28);
		buttonsPanel.add(submitBtn);
		
		add(buttonsPanel);
		
		bindEvent();
	}

	private void bindEvent() {
		loadBtn.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				connect();
			}
		});
		submitBtn.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				submit();
			}
		});
	}
	
	void connect() {		
		try {
			E4Config conf = TcpServer.getInstance().sendReqGetConf(tool.getKeyTextField().getText());
			setFormValues(conf);			
			Thread.sleep(1000);
		} catch (IOException e) {
			e.printStackTrace();
			tool.showMessage(e.getMessage(), Color.RED);
		} catch (Exception e) {
			e.printStackTrace();
			tool.showMessage(e.getMessage(), Color.RED);
		}
	}

	void setFormValues(E4Config props) {
		this.conf = props;
		
		newBrokerTextField.setText(props.getBroker());
		newSnTextField.setText(props.getSn());
		newWifiIdTextField.setText(props.getWifiId());
        newWifiPwTextField.setText(props.getWifiPass());
        
		for (int i = 0; i < newInputTypeComboBoxes.size(); i++) {
			if (props.getInputTypes().get(i) == 2) {
				newInputTypeComboBoxes.get(i).setSelectedItem(BUTTON);	
			}
			else {
				newInputTypeComboBoxes.get(i).setSelectedItem(SWITCH);
			}
		}
	}
	
	void setConnected() {
		newBrokerTextField.setEnabled(true);
		newSnTextField.setEnabled(true);
		for (int i = 0; i < newInputTypeComboBoxes.size(); i++) {
			newInputTypeComboBoxes.get(i).setEnabled(true);
		}
		loadBtn.setEnabled(true);
		submitBtn.setEnabled(true);
	}

	void setDisConnected() {
		newBrokerTextField.setEnabled(false);
		newSnTextField.setEnabled(false);
		for (int i = 0; i < newInputTypeComboBoxes.size(); i++) {
			newInputTypeComboBoxes.get(i).setEnabled(false);
		}
		loadBtn.setEnabled(false);
		submitBtn.setEnabled(false);
	}

	protected void submit() {
		String key = tool.getKeyTextField().getText();
		if (!tool.getMosquittoClient().isConnected()) {
			tool.showMessage("Cannot connect broker", Color.RED);
			return;
		}
		if (!tool.getMosquittoClient().isThingOnline()) {
			tool.showMessage("Cannot connect to '" + key + "'", Color.RED);
			return;
		}
		if (conf == null) {
			tool.showMessage("Please load config first", Color.RED);
			return;
		}
		if (!E4Config.validateSn(key)) {
			tool.showMessage("Key is not valid", Color.RED);
			return;
		}
		String newBroker = newBrokerTextField.getText();
		String newSn = newSnTextField.getText();
		if (!E4Config.validateSn(newSn)) {
			tool.showMessage("*SN is not valid", Color.RED);
			return;
		}
		String newWifiId = newWifiIdTextField.getText();
        if (!E4Config.validateWifiId(newWifiId)) {
            tool.showMessage("*Wifi ID is not valid", Color.RED);
            return;
        }
        String newWifiPw = newWifiPwTextField.getText();
        if (!E4Config.validateWifiPw(newWifiPw)) {
            tool.showMessage("*Wifi Pw is not valid", Color.RED);
            return;
        }
		List<Integer> newInputTypes = new ArrayList<>();
		for (int i = 0; i < newInputTypeComboBoxes.size(); i++) {
			if (newInputTypeComboBoxes.get(i).getSelectedIndex() == 1) {
				newInputTypes.add(2);
			}
			else {
				newInputTypes.add(1);
			}
		}
		
		try {
			
			if (!newBroker.equals(conf.getBroker())) {
				tool.showMessage("Updating Broker...", Color.BLACK);
				conf = TcpServer.getInstance().sendReqConfigSrvIp(key, newBroker);
				setFormValues(conf);
				tool.showMessage("Updated Broker !!!", Color.GREEN);
				Thread.sleep(1000);
			}
			
			if (!newWifiId.equals(conf.getWifiId())) {
                tool.showMessage("Updating Wifi ID...", Color.BLACK);
                conf = TcpServer.getInstance().sendReqConfigWifiId(key, newWifiId);
                setFormValues(conf);
                tool.showMessage("Updated Wifi ID !!!", Color.GREEN);
                Thread.sleep(1000);
            }
			
			if (!newWifiPw.equals(conf.getWifiPass())) {
                tool.showMessage("Updating Wifi Pw...", Color.BLACK);
                conf = TcpServer.getInstance().sendReqConfigWifiPw(key, newWifiPw);
                setFormValues(conf);
                tool.showMessage("Updated Wifi Pw !!!", Color.GREEN);
                Thread.sleep(1000);
            }
			
			if (!newSn.equals(conf.getSn()) && !ConfigTool.PUBLIC_MODE) {
				tool.showMessage("Updating SN...", Color.BLACK);
				conf = TcpServer.getInstance().sendReqConfigMac(key, newSn);
				setFormValues(conf);
				tool.showMessage("Updated SN !!!", Color.GREEN);
				Thread.sleep(1000);
			}
			
			for (int i = 0; i < newInputTypes.size(); i++) {
				if (newInputTypes.get(i).intValue() != conf.getInputTypes().get(i).intValue()) {
					tool.showMessage("Updating Input Type...", Color.BLACK);
					conf = TcpServer.getInstance().sendReqConfigInputTypes(key, newInputTypes);
					setFormValues(conf);
					tool.showMessage("Updated Input Type !!!", Color.GREEN);
					Thread.sleep(1000);
					break;
				}
			}

			tool.showMessage("Success. Please restart '" + key + "'", Color.GREEN);
		} catch (Throwable e) {
			e.printStackTrace();
			tool.showMessage(e.getMessage(), Color.RED);
		}
	}
	

	public void resetConfig() {
		for (JComboBox<String> combobox : newInputTypeComboBoxes) {
			combobox.setSelectedIndex(0);
		}
		newBrokerTextField.setText("");
		newSnTextField.setText("");
		newWifiIdTextField.setText("");
		newWifiPwTextField.setText("");
	}
}
