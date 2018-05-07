package net.ionoff.things.p4;

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

class ConfigPanel extends JPanel {
	
	private static final long serialVersionUID = 1L;
	
	private static final String SWITCH = "Switch";
	private static final String BUTTON = "Button";
	
	private P4Tool p8Tool;
	private P4Config conf;
	
	private JTextField newIpTextField;
	private JTextField newSubnetTextField;
	private JTextField newGatewayTextField;
	private JTextField newServerIpTextField;
	private JTextField newMacTextField;
	private List<JComboBox<String>> newInputTypeComboBoxes;
	private JComboBox<String> newInput1TypeComboBox;
	private JComboBox<String> newInput2TypeComboBox;
	private JComboBox<String> newInput3TypeComboBox;
	private JComboBox<String> newInput4TypeComboBox;
	private JComboBox<String> newInput5TypeComboBox;
	private JComboBox<String> newInput6TypeComboBox;
	private JComboBox<String> newInput7TypeComboBox;
	private JComboBox<String> newInput8TypeComboBox;

	private JButton loadBtn;
	private JButton submitBtn;

	ConfigPanel(P4Tool p8Tool) {
		this.p8Tool = p8Tool;
		setLayout(null);
		
		//////////////// --------------------------------------------------------------------
		JPanel newIpPwPanel = new JPanel();
		newIpPwPanel.setBounds(0, 10, 365, 35);
		newIpPwPanel.setLayout(null);

		JLabel newIpLbl = new JLabel("IP");
		newIpLbl.setBounds(10, 5, 65, 25);
		newIpPwPanel.add(newIpLbl);
		newIpTextField = new JTextField(20);
		newIpTextField.setBounds(65, 5, 285, 25);
		newIpPwPanel.add(newIpTextField);

		add(newIpPwPanel);
		
		//////////////// --------------------------------------------------------------------
		JPanel newSubnetPanel = new JPanel();
		newSubnetPanel.setBounds(0, 45, 365, 35);
		newSubnetPanel.setLayout(null);
		JLabel newSubnetLbl = new JLabel("Subnet");
		newSubnetLbl.setBounds(10, 5, 65, 25);
		newSubnetPanel.add(newSubnetLbl);
		newSubnetTextField = new JTextField(20);
		newSubnetTextField.setBounds(65, 5, 285, 25);
		newSubnetPanel.add(newSubnetTextField);
		add(newSubnetPanel);

		//////////////// --------------------------------------------------------------------
		JPanel newGatewayPanel = new JPanel();
		newGatewayPanel.setBounds(0, 80, 365, 35);
		newGatewayPanel.setLayout(null);
		JLabel newGatewayLbl = new JLabel("Gateway");
		newGatewayLbl.setBounds(10, 5, 65, 25);
		newGatewayPanel.add(newGatewayLbl);
		newGatewayTextField = new JTextField(20);
		newGatewayTextField.setBounds(65, 5, 285, 25);
		newGatewayPanel.add(newGatewayTextField);
		add(newGatewayPanel);

		//////////////// --------------------------------------------------------------------
		JPanel newInputTypePanel = new JPanel();
		newInputTypePanel.setBounds(0, 115, 365, 70);

		newInputTypePanel.setLayout(null);
		JLabel newInputTypePanelLbl = new JLabel("Input");
		newInputTypePanelLbl.setBounds(10, 5, 65, 25);
		newInputTypePanel.add(newInputTypePanelLbl);
		
		newInputTypeComboBoxes = new ArrayList<JComboBox<String>>();
		
		JLabel newInput1TypePanelLbl = new JLabel("1");
		newInput1TypePanelLbl.setHorizontalAlignment(SwingConstants.CENTER);
		newInput1TypePanelLbl.setBounds(65, 5, 10, 25);
		newInputTypePanel.add(newInput1TypePanelLbl);
		
		newInput1TypeComboBox = new JComboBox<>();
		newInput1TypeComboBox.addItem(SWITCH);
		newInput1TypeComboBox.addItem(BUTTON);
		newInput1TypeComboBox.setBounds(75, 5, 61, 25);
		newInputTypeComboBoxes.add(newInput1TypeComboBox);
		newInputTypePanel.add(newInput1TypeComboBox);
		
		JLabel newInput2TypePanelLbl = new JLabel("2");
		newInput2TypePanelLbl.setHorizontalAlignment(SwingConstants.CENTER);
		newInput2TypePanelLbl.setBounds(136, 5, 10, 25);
		newInputTypePanel.add(newInput2TypePanelLbl);
		
		newInput2TypeComboBox = new JComboBox<>();
		newInput2TypeComboBox.addItem(SWITCH);
		newInput2TypeComboBox.addItem(BUTTON);
		newInput2TypeComboBox.setBounds(146, 5, 61, 25);
		newInputTypeComboBoxes.add(newInput2TypeComboBox);
		newInputTypePanel.add(newInput2TypeComboBox);
		
		JLabel newInput3TypePanelLbl = new JLabel("3");
		newInput3TypePanelLbl.setHorizontalAlignment(SwingConstants.CENTER);
		newInput3TypePanelLbl.setBounds(207, 5, 10, 25);
		newInputTypePanel.add(newInput3TypePanelLbl);
		
		newInput3TypeComboBox = new JComboBox<>();
		newInput3TypeComboBox.addItem(SWITCH);
		newInput3TypeComboBox.addItem(BUTTON);
		newInput3TypeComboBox.setBounds(217, 5, 61, 25);
		newInputTypeComboBoxes.add(newInput3TypeComboBox);
		newInputTypePanel.add(newInput3TypeComboBox);
		
		JLabel newInput4TypePanelLbl = new JLabel("4");
		newInput4TypePanelLbl.setHorizontalAlignment(SwingConstants.CENTER);
		newInput4TypePanelLbl.setBounds(278, 5, 10, 25);
		newInputTypePanel.add(newInput4TypePanelLbl);
		
		newInput4TypeComboBox = new JComboBox<>();
		newInput4TypeComboBox.addItem(SWITCH);
		newInput4TypeComboBox.addItem(BUTTON);
		newInput4TypeComboBox.setBounds(288, 5, 62, 25);
		newInputTypeComboBoxes.add(newInput4TypeComboBox);
		newInputTypePanel.add(newInput4TypeComboBox);

		////////////////////////////////////////////////////////////////////
		JLabel newInput5TypePanelLbl = new JLabel("5");
		newInput5TypePanelLbl.setHorizontalAlignment(SwingConstants.CENTER);
		newInput5TypePanelLbl.setBounds(65, 35, 10, 25);
		newInputTypePanel.add(newInput5TypePanelLbl);
		newInput5TypePanelLbl.setVisible(false);
		
		newInput5TypeComboBox = new JComboBox<>();
		newInput5TypeComboBox.addItem(SWITCH);
		newInput5TypeComboBox.addItem(BUTTON);
		newInput5TypeComboBox.setBounds(75, 35, 61, 25);
		newInputTypeComboBoxes.add(newInput5TypeComboBox);
		newInputTypePanel.add(newInput5TypeComboBox);
		newInput5TypeComboBox.setVisible(false);
		
		JLabel newInput6TypePanelLbl = new JLabel("6");
		newInput6TypePanelLbl.setHorizontalAlignment(SwingConstants.CENTER);
		newInput6TypePanelLbl.setBounds(136, 35, 10, 25);
		newInputTypePanel.add(newInput6TypePanelLbl);
		newInput6TypePanelLbl.setVisible(false);
		
		newInput6TypeComboBox = new JComboBox<>();
		newInput6TypeComboBox.addItem(SWITCH);
		newInput6TypeComboBox.addItem(BUTTON);
		newInput6TypeComboBox.setBounds(146, 35, 61, 25);
		newInputTypeComboBoxes.add(newInput6TypeComboBox);
		newInputTypePanel.add(newInput6TypeComboBox);
		newInput6TypeComboBox.setVisible(false);
		
		JLabel newInput7TypePanelLbl = new JLabel("7");
		newInput7TypePanelLbl.setHorizontalAlignment(SwingConstants.CENTER);
		newInput7TypePanelLbl.setBounds(207, 35, 10, 25);
		newInputTypePanel.add(newInput7TypePanelLbl);
		newInput7TypePanelLbl.setVisible(false);
		
		newInput7TypeComboBox = new JComboBox<>();
		newInput7TypeComboBox.addItem(SWITCH);
		newInput7TypeComboBox.addItem(BUTTON);
		newInput7TypeComboBox.setBounds(217, 35, 61, 25);
		newInputTypeComboBoxes.add(newInput7TypeComboBox);
		newInputTypePanel.add(newInput7TypeComboBox);
		newInput7TypeComboBox.setVisible(false);
		
		JLabel newInput8TypePanelLbl = new JLabel("8");
		newInput8TypePanelLbl.setHorizontalAlignment(SwingConstants.CENTER);
		newInput8TypePanelLbl.setBounds(278, 35, 10, 25);
		newInputTypePanel.add(newInput8TypePanelLbl);
		newInput8TypePanelLbl.setVisible(false);
		
		newInput8TypeComboBox = new JComboBox<>();
		newInput8TypeComboBox.addItem(SWITCH);
		newInput8TypeComboBox.addItem(BUTTON);
		newInput8TypeComboBox.setBounds(288, 35, 62, 25);
		newInputTypeComboBoxes.add(newInput8TypeComboBox);
		newInputTypePanel.add(newInput8TypeComboBox);
		newInput8TypeComboBox.setVisible(false);
		
		add(newInputTypePanel);
		

		//////////////// --------------------------------------------------------------------
		JPanel newServerIpMacPanel = new JPanel();
		newServerIpMacPanel.setBounds(0, 180, 365, 45);
		newServerIpMacPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
		newServerIpMacPanel.setLayout(null);

		JLabel newServerIpLbl = new JLabel("Server");
		newServerIpLbl.setBounds(10, 5, 65, 25);
		newServerIpMacPanel.add(newServerIpLbl);
		newServerIpTextField = new JTextField(20);
		if (P4Tool.PUBLIC_MODE) {
			newServerIpTextField.setBounds(65, 5, 285, 25);
		}
		else {
			newServerIpTextField.setBounds(65, 5, 120, 25);
		}
		newServerIpMacPanel.add(newServerIpTextField);

		JLabel newMacLbl = new JLabel("MAC");
		newMacLbl.setBounds(195, 5, 40, 25);
		if (P4Tool.PUBLIC_MODE) {
			newMacLbl.setVisible(false);
		}
		newServerIpMacPanel.add(newMacLbl);
		newMacTextField = new JTextField(20);
		newMacTextField.setBounds(230, 5, 120, 25);
		if (P4Tool.PUBLIC_MODE) {
			newMacTextField.setVisible(false);
		}
		newServerIpMacPanel.add(newMacTextField);

		add(newServerIpMacPanel);

		//////////////// --------------------------------------------------------------------
		JPanel buttonsPanel = new JPanel();
		buttonsPanel.setBounds(0, 235, 365, 45);
		buttonsPanel.setLayout(null);
		
		loadBtn = new JButton("Load");
		loadBtn.setBounds(126, 10, 90, 25);
		buttonsPanel.add(loadBtn);
		
		submitBtn = new JButton("Submit");
		submitBtn.setBounds(226, 10, 125, 27);
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
			P4Config conf = TcpServer.getInstance().sendReqGetConf(p8Tool.getPwTextField().getText());
			setFormValues(conf);			
			Thread.sleep(1000);
		} catch (IOException e) {
			e.printStackTrace();
			p8Tool.showMessage(e.getMessage(), Color.RED);
		} catch (Exception e) {
			e.printStackTrace();
			p8Tool.showMessage(e.getMessage(), Color.RED);
		}
	}

	void setFormValues(P4Config props) {
		this.conf = props;
		
		newIpTextField.setText(props.getIp());
		newSubnetTextField.setText(props.getSubnetMask());
		newGatewayTextField.setText(props.getGateway());
		newServerIpTextField.setText(props.getSrvIp());
		newMacTextField.setText(props.getMac());
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
		newIpTextField.setEnabled(true);
		newSubnetTextField.setEnabled(true);
		newGatewayTextField.setEnabled(true);
		newServerIpTextField.setEnabled(true);
		newMacTextField.setEnabled(true);
		for (int i = 0; i < newInputTypeComboBoxes.size(); i++) {
			newInputTypeComboBoxes.get(i).setEnabled(true);
		}
		loadBtn.setEnabled(true);
		submitBtn.setEnabled(true);
	}

	void setDisConnected() {
		newIpTextField.setEnabled(false);
		newSubnetTextField.setEnabled(false);
		newGatewayTextField.setEnabled(false);
		newServerIpTextField.setEnabled(false);
		newMacTextField.setEnabled(false);
		for (int i = 0; i < newInputTypeComboBoxes.size(); i++) {
			newInputTypeComboBoxes.get(i).setEnabled(false);
		}
		loadBtn.setEnabled(false);
		submitBtn.setEnabled(false);
	}

	protected void submit() {

		if (conf == null) {
			p8Tool.showMessage("No connection", Color.RED);
			return;
		}
		
		String ip = p8Tool.getIpTextField().getText();
		if (!P4Config.validateIp(ip)) {
			p8Tool.showMessage("IP is not valid", Color.RED);
			return;
		}
		String pw = p8Tool.getPwTextField().getText();
		if (!P4Config.validatePw(pw)) {
			p8Tool.showMessage("PW is not valid", Color.RED);
			return;
		}
		
		String newIp = newIpTextField.getText();
		if (!P4Config.validateIp(newIp)) {
			p8Tool.showMessage("*IP is not valid", Color.RED);
			return;
		}
		
		String newSubnet = newSubnetTextField.getText();
		if (!P4Config.validateIp(newSubnet)) {
			p8Tool.showMessage("*Subnet is not valid", Color.RED);
			return;
		}
		String newGateway = newGatewayTextField.getText();
		if (!P4Config.validateIp(newGateway)) {
			p8Tool.showMessage("*Gateway is not valid", Color.RED);
			return;
		}
		String newSrvIp = newServerIpTextField.getText();
		if (!P4Config.validateIp(newSrvIp)) {
			p8Tool.showMessage("*Server is not valid", Color.RED);
			return;
		}
		String newMac = newMacTextField.getText();
		if (!P4Config.validateMac(newMac)) {
			p8Tool.showMessage("*MAC is not valid", Color.RED);
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
			if (!newIp.equals(conf.getIp())) {
				p8Tool.showMessage("Updating IP...", Color.BLACK);
				conf = TcpServer.getInstance().sendReqConfigIp(newIp, pw);
				setFormValues(conf);
				p8Tool.showMessage("Updated IP !!!", Color.GREEN);
				Thread.sleep(1000);
			}
			
			if (!newSubnet.equals(conf.getSubnetMask())) {
				p8Tool.showMessage("Updating Subnet...", Color.BLACK);
				conf = TcpServer.getInstance().sendReqConfigSubnetMask(newSubnet, pw);
				setFormValues(conf);
				p8Tool.showMessage("Updated Subnet !!!", Color.GREEN);
				Thread.sleep(1000);
			}
			
			if (!newGateway.equals(conf.getGateway())) {
				p8Tool.showMessage("Updating Gateway...", Color.BLACK);
				conf = TcpServer.getInstance().sendReqConfigGateway(newGateway, pw);
				setFormValues(conf);
				p8Tool.showMessage("Updated Gateway !!!", Color.GREEN);
				Thread.sleep(1000);
			}
			
			if (!newSrvIp.equals(conf.getSrvIp())) {
				p8Tool.showMessage("Updating SrvIp...", Color.BLACK);
				conf = TcpServer.getInstance().sendReqConfigSrvIp(newSrvIp, pw);
				setFormValues(conf);
				p8Tool.showMessage("Updated SrvIp !!!", Color.GREEN);
				Thread.sleep(1000);
			}
			
			if (!newMac.equals(conf.getMac()) && !P4Tool.PUBLIC_MODE) {
				p8Tool.showMessage("Updating MAC...", Color.BLACK);
				conf = TcpServer.getInstance().sendReqConfigMac(newMac, pw);
				setFormValues(conf);
				p8Tool.showMessage("Updated PW !!!", Color.GREEN);
				Thread.sleep(1000);
			}
			
			for (int i = 0; i < newInputTypes.size(); i++) {
				if (newInputTypes.get(i).intValue() != conf.getInputTypes().get(i).intValue()) {
					p8Tool.showMessage("Updating Input Type...", Color.BLACK);
					conf = TcpServer.getInstance().sendReqConfigInputTypes(newInputTypes, pw);
					setFormValues(conf);
					p8Tool.showMessage("Updated Input Type !!!", Color.GREEN);
					Thread.sleep(1000);
					break;
				}
			}

			p8Tool.showMessage("Success. Please restart relayDriver", Color.GREEN);
		} catch (Throwable e) {
			e.printStackTrace();
			p8Tool.showMessage(e.getMessage(), Color.RED);
		}
	}
	

	public void resetConfig() {
		newIpTextField.setText("");
		newSubnetTextField.setText("");
		newGatewayTextField.setText("");
		for (JComboBox<String> combobox : newInputTypeComboBoxes) {
			combobox.setSelectedIndex(0);
		}
		newServerIpTextField.setText("");
		newMacTextField.setText("");
	}
}
