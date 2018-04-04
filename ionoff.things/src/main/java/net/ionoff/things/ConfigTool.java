package net.ionoff.things;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import com.alee.laf.WebLookAndFeel;

import net.ionoff.things.e4.E4Config;
import net.ionoff.things.e4.E4ConfigPanel;
import net.ionoff.things.e4.E4Status;
import net.ionoff.things.e4.E4TestPanel;
import net.ionoff.things.mqtt.MosquittoClient;
import net.ionoff.things.mqtt.UserConfig;

public class ConfigTool extends JFrame {

	private static final long serialVersionUID = 1L;
	public static final boolean PUBLIC_MODE = false; 
	public static final String IONOFF_E4 = "IONOFF_E4"; 
	public static final String IONOFF_F8 = "IONOFF_F8"; 
	
	public static final Color UNKNOWN_BROKER_COLOR =  new Color(217, 217, 217);
	public static final Color DISCONNECT_BROKER_COLOR =  new Color(255, 188, 188);
	public static final Color CONNECTED_BROKER_COLOR =  new Color(228, 255, 209);
	
	private JComboBox<String> modelComboBox;
	private JTextField keyTextField;
	private JButton connectBtn;
	private JPanel messagePanel;
	
	private JLabel messageLbl;
	private JTabbedPane tabbedPane;
	private E4ConfigPanel confPanel;
	private E4TestPanel controlPanel;
	
	private MosquittoClient mosquittoClient;

	// Create a form with the fields
	public ConfigTool() {

		super("IOnOff Things Configuration Tool");

		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setSize(540, 350);
		setResizable(false);

		JMenuBar menuBar = new JMenuBar();

		JMenu fileMenu = new JMenu("File");
		menuBar.add(fileMenu);
		JMenuItem quitMenuItem = new JMenuItem("Quit");
		quitMenuItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				System.exit(0);
			}
		});
		fileMenu.add(quitMenuItem);
		setJMenuBar(menuBar);

		JPanel contentPane = new JPanel();
		contentPane.setLayout(null);
		this.setContentPane(contentPane);
		
		
		////////////////--------------------------------------------------------------------
		JPanel setupPanel = new JPanel();
		setupPanel.setBounds(0, 0, 540, 55);
		setupPanel.setBackground(new Color(217, 217, 217));
		setupPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
		setupPanel.setLayout(null);
		JLabel ipLbl = new JLabel("Model");
		ipLbl.setBounds(10, 15, 55, 25);
		setupPanel.add(ipLbl);
		modelComboBox = new JComboBox<>();
		modelComboBox.addItem(IONOFF_E4);
		modelComboBox.addItem(IONOFF_F8);
		modelComboBox.setBounds(65, 14, 100, 26);
		setupPanel.add(modelComboBox);
		
		JLabel keyLbl = new JLabel("Key");
		keyLbl.setBounds(190, 15, 35, 25);
		setupPanel.add(keyLbl);
		keyTextField = new JTextField(20);
		keyTextField.setText("");
		keyTextField.addKeyListener(new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				messageLbl.setText("");
			}
			
			@Override
			public void keyReleased(KeyEvent e) {
				//
			}
			
			@Override
			public void keyPressed(KeyEvent e) {
				//
			}
		});
		
		keyTextField.setBounds(220, 15, 140, 26);
		setupPanel.add(keyTextField);
		
		connectBtn = new JButton("Connect");
		connectBtn.setBounds(400, 14, 125, 28);
		setupPanel.add(connectBtn);
		
		connectBtn.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String key = keyTextField.getText();
				if (!validateKey(key)) {
					return;
				}
				mosquittoClient.connectBroker();
				mosquittoClient.setPublishTopic(UserConfig.MQTT_TOPIC_MSG_FORMAT.replaceAll("%id", key));
				mosquittoClient.setSubscribleTopic(UserConfig.MQTT_TOPIC_CMD_FORMAT.replaceAll("%id", key));
			}
		});
		
		contentPane.add(setupPanel);
		
		////////////////--------------------------------------------------------------------
		messagePanel = new JPanel();
		messagePanel.setBackground(UNKNOWN_BROKER_COLOR);
		messagePanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
		messagePanel.setBounds(0, 55, 540, 25);
		messagePanel.setLayout(null);
		messageLbl = new JLabel("");
		messageLbl.setHorizontalAlignment(SwingConstants.CENTER);
		messageLbl.setBounds(10, 0, 345, 25);
		messagePanel.add(messageLbl);
		contentPane.add(messagePanel);
		
		////////////////--------------------------------------------------------------------
		confPanel = new E4ConfigPanel(this);
		controlPanel = new E4TestPanel(this);
		////////////////--------------------------------------------------------------------
		tabbedPane = new JTabbedPane();
		tabbedPane.setBounds(0, 90, 535, 315);
		
		tabbedPane.addTab("Config", confPanel);
		tabbedPane.addTab("Test", controlPanel);
		
		contentPane.add(tabbedPane);
		
		
		////////////////--------------------------------------------------------------------
		setLocationRelativeTo(null);
		this.setVisible(true);
		
		mosquittoClient = new MosquittoClient() {

			@Override
			public void onConnectedBroker() {
				messagePanel.setBackground(CONNECTED_BROKER_COLOR);
			}

			@Override
			public void onConnectionLost() {
				messagePanel.setBackground(DISCONNECT_BROKER_COLOR);
			}

			@Override
			public void onMessageArrived(String topic, String message) {
				// TODO Auto-generated method stub
				
			}			
		};
	}
	
	boolean validateKey(String key) {
		if (key == null || key.isEmpty() || (!key.startsWith("E4") && key.startsWith("F8"))
				|| key.length() != 16) {
			showMessage("Key is not valid", Color.RED);
			return false;
		}
		return true;
	}

	public void showMessage(final String string, final Color color) {
		messageLbl.setText(string);
		messageLbl.setForeground(color);
		messageLbl.repaint();
	}

	public static void main(String[] args) {
		WebLookAndFeel.install();
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				ConfigTool p8Tool = new ConfigTool();
				TcpServer.getInstance().setP8Tool(p8Tool);
				TcpServer.getInstance().start();
			}
		});
	}

	
	public boolean validateKey() {
		return E4Config.validateSn(keyTextField.getText());
	}

	public JTextField getKeyTextField() {
		return keyTextField;
	}

	public void setStatus(E4Status status) {
		controlPanel.setStatus(status);
	}
	
	public void resetConfig() {
		confPanel.resetConfig();
	}

	public MosquittoClient getMosquittoClient() {
		return mosquittoClient;
	}
}